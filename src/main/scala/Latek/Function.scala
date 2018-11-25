package Latek
import Latek.Language._
import scala.collection.JavaConversions._
import scala.collection.mutable.{Map, Set, ArrayBuffer}

class Variable(val name : String, val varType : Type) {
	if(!varType.instantiable) {
		throw new CompilationException("Type " + varType.name + " cannot be instantiated.")
	}

	def llvmName : String = varType.llvmName + " " + llvmReg
	def llvmPtr : String = varType.llvmPtr + " " + llvmReg
	def llvmReg : String = ""
	def localIndex = 0
}

class LocalVariable(name : String, varType : Type, val func : UserFunction) extends Variable(name, varType) {
	var index = 0

	override def localIndex = index

	override def llvmReg : String = func.localRegister(this.index)
}

abstract class Function(val program : Program, val name : String) {
	def funcType : TypeFunction
	def returnType : Type
	def resolve = {}
	def compile = {}
	def llvmName : String = "@"+name
	override def toString = name + " : " + funcType

	def argRegister(idx : Integer) = "%arg"+idx

	def header = (returnType.llvmName+" "+llvmName+
		"(" + funcType.argTypes.zipWithIndex.map(
		{case (tp : Type, idx : Int) => tp.llvmName+" "+argRegister(idx)}).mkString(", ") + ")")

	def headerDefine = "define " + header + " {"
	def headerDeclare = "declare " + header
	def isExternal = false
	def code : Seq[String] = List()

}

class NativeFunction(program : Program, name : String, private val ft : TypeFunction)
	extends Function(program, name) {
	override def funcType = ft
	override def returnType = ft.returnType
	override def isExternal = true
}

class UserFunction(program : Program, val funcDef : LangFuncDef) extends Function(program, funcDef.name) {
	val scope = new Scope(program.globalScope, this)
	val arguments = funcDef.arguments.toList.map(
		(arg) => new LocalVariable(arg.name, program.getType(arg.typeName), this))

	val locals = ArrayBuffer[LocalVariable]()
	protected var _labelIndex = 0

	protected val _returnType = program.getType(funcDef.returnTypeName)
	protected val _funcType : TypeFunction = new TypeFunction(returnType, arguments.map((v) => {v.varType}))
	protected val _code = new ArrayBuffer[String]()

	override def funcType : TypeFunction = _funcType
	override def returnType : Type = _returnType

	def localRegister(idx : Integer) = "%loc"+idx
	def localRegister(v : LocalVariable) : String = localRegister(v.index)

	def addLocal(v : LocalVariable) = {
		locals += v
		v.index = locals.size - 1
	}

	def newBlock : BasicBlock = {
		val bl = new BasicBlock(this, _labelIndex)
		_labelIndex += 1

		return bl
	}

	// def addBlocks(bls : Seq[BasicBlock]) = {
	// 	for(b <- bls) {
	// 		basicBlocks += b
	// 		b.index = basicBlocks.size - 1
	// 	}
	// }

	override def resolve = {
		try {
			// add arguments to scope
			for(arg <- arguments) {
				addLocal(arg)
				scope.addVariable(arg.name, arg)
			}

			for(stmt <- funcDef.body.stmts) {
				stmt.resolve(scope)
			}
		} catch {
			case ex : CompilationException => {
				ex.setFunc(this)
				throw ex
			}
		}
	}

	def buildConditionalAnd(a : LangExp, b : LangExp, blockIfTrue : BasicBlock, blockIfFalse : BasicBlock) : Seq[BasicBlock] = {
		val bl = newBlock
		val (expValA, instrsA) = a.compile(bl)
		val (expValB, instrsB) = b.compile(bl)

		return expValA match {
			case ValueConstantBool(constA) => {
				// a is const
				if(constA) {
					// true && b = b
					buildConditional(b, blockIfTrue, blockIfFalse)
				} else {
					// false && b = false
					bl.branchTo(blockIfFalse)
					List(bl)
				}
			}
			case _ => expValB match {
				case ValueConstantBool(constB) => {
					if(constB) {
						// a && true = a
						buildConditional(a, blockIfTrue, blockIfFalse)
					} else {
						// b && false = false
						if(a.hasSideEffects) {
							bl.addInstructions(instrsA)
						}
						bl.branchTo(blockIfFalse)
						List(bl)
					}
				}
				case _ => {
					val blocks_b = buildConditional(b, blockIfTrue, blockIfFalse)
					val blocks_a = buildConditional(a, blocks_b.head, blockIfFalse)
					blocks_a ++ blocks_b
				}
			}
		}
	}

	def buildConditionalOr(a : LangExp, b : LangExp, blockIfTrue : BasicBlock, blockIfFalse : BasicBlock) : Seq[BasicBlock] = {
		val bl = newBlock
		val (expValA, instrsA) = a.compile(bl)
		val (expValB, instrsB) = b.compile(bl)

		return expValA match {
			case ValueConstantBool(constA) => {
				// a is const
				if(constA) {
					// true || b = true
					bl.branchTo(blockIfTrue)
					List(bl)
				} else {
					// false || b = b
					buildConditional(b, blockIfTrue, blockIfFalse)
				}
			}
			case _ => expValB match {
				case ValueConstantBool(constB) => {
					if(constB) {
						// a || true = true
						if(a.hasSideEffects) {
							bl.addInstructions(instrsA)
						}
						bl.branchTo(blockIfTrue)
						List(bl)

					} else {
						// a || false = a
						buildConditional(a, blockIfTrue, blockIfFalse)
					}
				}
				case _ => {
					val blocks_b = buildConditional(b, blockIfTrue, blockIfFalse)
					val blocks_a = buildConditional(a, blockIfTrue, blocks_b.head)
					blocks_a ++ blocks_b
				}
			}
		}
	}

	def buildConditional(cond : LangExp, blockIfTrue : BasicBlock, blockIfFalse : BasicBlock) : Seq[BasicBlock] = {
		// attempt calculating compile-time constants
		//println("build cond "+cond)

		return cond match {
			case LangExp_And(a, b) => {
				buildConditionalAnd(a, b, blockIfTrue, blockIfFalse)
			}
			case LangExp_Or(a, b) => {
				buildConditionalOr(a, b, blockIfTrue, blockIfFalse)
			}
			case LangExp_Not(a) => {
				// swap blockIfTrue and blockIfFalse
				buildConditional(a, blockIfFalse, blockIfTrue)
			}

			case _ => {
				val bl = newBlock
				val (expVal, instrs) = cond.compile(bl)

				expVal match {
					case ValueConstantBool(constVal) => {
						if(constVal) {
							//println("const cond, going to "+blockIfTrue)
							bl.branchTo(blockIfTrue)
							List(bl)
						} else {
							//println("const cond, going to "+blockIfFalse)
							bl.branchTo(blockIfFalse)
							List(bl)
						}
					}
					case _ => {
						bl.addInstructions(instrs)
						bl.addInstruction(new InstrBranchConditional(bl, expVal, blockIfTrue, blockIfFalse))
						bl.branchTo(blockIfTrue)
						bl.branchTo(blockIfFalse)
						List(bl)
					}
				}
			}
		}
	}

	def compileStatements(stmts : Seq[LangStmt], entryBlock : BasicBlock, exitBlock : BasicBlock) : ArrayBuffer[BasicBlock] = {
		val blocks = new ArrayBuffer[BasicBlock]()

		var prevExitBlock = entryBlock

		for(stmt <- stmts) {
			stmt.createBlocks(this)

			if(prevExitBlock != null) {
				prevExitBlock.branchTo(stmt.entryBlock)
			}
			prevExitBlock = stmt.exitBlock

			blocks ++= stmt.blocks
		}

		if(prevExitBlock != null && exitBlock != null) {
			prevExitBlock.branchTo(exitBlock)
		}

		return blocks
	}

	override def compile = {
		try {
			val init_block = newBlock
			val exit_block = newBlock
			val body_blocks = compileStatements(funcDef.body.stmts, init_block, null)

			for((loc, idx) <- locals.zipWithIndex) {
				init_block.addInstruction(new InstrAlloca(init_block, localRegister(idx), loc.varType))

				if(idx < arguments.size) {
					init_block.addInstruction(new InstrStore(init_block, loc, ValueRegister(argRegister(idx))))
					//init_block.addInstruction(new InstrSetLocal(init_block, idx, ValueRegister(argRegister(idx))))
				}
			}

			//discover visible blocks
			val allBlocks = init_block.collectControlGraph

			// val allBlocks = ArrayBuffer[BasicBlock]()
			// allBlocks += init_block
			// allBlocks ++= body_blocks
			// allBlocks += exit_block

			for(bl <- allBlocks) {
				bl.processValues
			}

			_code += ""
			_code += headerDefine

			for(bl <- allBlocks) {
				bl.generateConnections

				for(instr <- bl.instructions) {
					//instr.allocateGlobals(program)

					_code += "	"+instr.toString
				}
			}

			_code += "}"
			_code += ""
		} catch {
			case ex : CompilationException => {
				ex.setFunc(this)
				throw ex
			}
		}
	}

	override def code = _code
}

class Method(program : Program, funcDef : LangFuncDef, val cls : Class) extends UserFunction(program, funcDef) {
	override val scope = new Scope(cls.scope, this)
}

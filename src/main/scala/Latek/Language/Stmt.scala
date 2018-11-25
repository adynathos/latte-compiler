package Latek.Language
import Latek._
import java.util.LinkedList
import scala.collection.JavaConversions._
import scala.collection.mutable.ArrayBuffer

abstract class LangStmt() {
	def resolve(scope : Scope) = {}
	def createBlocks(func : UserFunction) = {}

	def hasSideEffects = false

	// many statements are single-block, so let this be default
	protected var _block : BasicBlock = null

	// basic blocks
	def blocks : Seq[BasicBlock] = List(_block)
	def entryBlock : BasicBlock = _block
	def exitBlock : BasicBlock = _block

}

case class LangStmt_Empty() extends LangStmt

case class LangStmt_Value(val exp: LangExp) extends LangStmt {
	override def hasSideEffects = exp.hasSideEffects

	override def resolve(scope : Scope) {
		exp.resolve(scope)
	}

	override def createBlocks(func : UserFunction) = {
		_block = func.newBlock

		val (expVal, instrs) = exp.compile(_block)
		_block.addInstructions(instrs)
	}
}

case class LangStmt_Assign(val lvalue: LangLValue, val exp: LangExp) extends LangStmt {
	override def hasSideEffects = true

	override def resolve(scope : Scope) {
		lvalue.resolve(scope)
		exp.resolve(scope)

		if(! exp.expType.isA(lvalue.expType)) {
			throw new CompilationException("Cannot assign value of type "+exp.expType
				+ " to a variable of type "+lvalue.expType)
		}
	}

	override def createBlocks(func : UserFunction) = {
		_block = func.newBlock

		val (expVal, instrs) = exp.compile(_block)
		_block.addInstructions(instrs)

		var vr = lvalue.variable

		//_block.addInstruction(new InstrSetLocal(_block, vr.localIndex, expVal))
		_block.addInstruction(new InstrStore(_block, vr, expVal))
		//_block.localValues(vr.localIndex) = expVal
	}
}

abstract class LangStmt_RelativeOperation(val valueExp: LangLValue) extends LangStmt {
	protected var _stmt = LangStmt_Assign(valueExp, LangExp_Add(valueExp.exp, relativeOperation, LangExp_LitInt(1)))

	protected def relativeOperation : LangOpAdd
	protected def relativeOperationName : String

	override def hasSideEffects = true

	override def resolve(scope : Scope) {
		valueExp.resolve(scope)

		valueExp.expType match {
			case TypeInt() => {
				//ok
			}
			case _ => {
				throw new CompilationException("Cannot use "+relativeOperationName+" on value of type "+valueExp.expType)
			}
		}

		_stmt.resolve(scope)
	}

	override def createBlocks(func : UserFunction) = {
		_stmt.createBlocks(func)
	}

	override def blocks : Seq[BasicBlock] = _stmt.blocks
	override def entryBlock : BasicBlock = _stmt.entryBlock
	override def exitBlock : BasicBlock = _stmt.exitBlock
}

case class LangStmt_AddOne(exp: LangLValue) extends LangStmt_RelativeOperation(exp) {
	override def relativeOperation = LangOpAdd_Add()
	override def relativeOperationName = "++"
}

case class LangStmt_SubOne(exp: LangLValue) extends LangStmt_RelativeOperation(exp) {
	override def relativeOperation = LangOpAdd_Sub()
	override def relativeOperationName = "--"
}

case class LangStmt_Block(block: LangFuncBlock) extends LangStmt {
	var blockScope : Scope = null
	protected var blStart : BasicBlock = null
	protected var blEnd : BasicBlock = null
	protected var _blocks = new ArrayBuffer[BasicBlock]()

	override def entryBlock = blStart
	override def exitBlock = blEnd
	override def blocks = _blocks

	override def hasSideEffects : Boolean = {
		for(stmt <- block.stmts.toList) {
			if(stmt.hasSideEffects) {
				return true
			}
		}
		return false
	}

	override def resolve(scope : Scope) {
		blockScope = new Scope(scope)

		for(stmt <- block.stmts.toList) {
			stmt.resolve(blockScope)
		}
	}

	override def createBlocks(func : UserFunction) = {
		_blocks ++= func.compileStatements(block.stmts.toList, null, null)

		if(_blocks.size == 0) {
			_blocks += func.newBlock
		}

		blStart = _blocks(0)
		blEnd = _blocks(_blocks.size-1)
	}
}

case class LangFuncBlock(val stmts: ListLangStmt)
case class ListLangStmt() extends LinkedList[LangStmt]


case class LangStmt_Declaration(typeName: String, declarations: ListLangVarDeclaration) extends LangStmt {
	protected var varsWithInit = new ArrayBuffer[(LocalVariable, LangExp)]()

	override def hasSideEffects = true

	override def resolve(scope : Scope) {
		val tp = scope.program.getType(typeName)

		for(vd <- declarations.toList) {
			var vname = ""
			var init_exp : LangExp = null

			vd match {
				case LangVarDeclaration_Init(varName) => {
					vname = varName
				}

				case LangVarDeclaration_NoInit(varName, initValueExp) => {
					vname = varName
					initValueExp.resolve(scope)
					init_exp = initValueExp

					if(! initValueExp.expType.isA(tp)) {
						throw new CompilationException("Cannot assign value of type "+initValueExp.expType
							+ " to a variable of type "+tp)
					}
				}
			}

			var v = new LocalVariable(vname, tp, scope.owningFunc)
			scope.addVariable(vname, v)
			scope.owningFunc.addLocal(v)

			if(init_exp == null) {
				init_exp = v.varType.baseExp
			}

			varsWithInit += ((v, init_exp))
		}
	}

	override def createBlocks(func : UserFunction) = {
		_block = func.newBlock

		for((v, init_exp) <- varsWithInit) {
			val (expVal, instrs) = init_exp.compile(_block)
			_block.addInstructions(instrs)
			_block.addInstruction(new InstrStore(_block, v, expVal))
			//_block.addInstruction(new InstrSetLocal(_block, v.localIndex, expVal))
		}
	}
}

class LangVarDeclaration()
case class LangVarDeclaration_Init(varName: String) extends LangVarDeclaration
case class LangVarDeclaration_NoInit(varName: String, initValueExp: LangExp) extends LangVarDeclaration
case class ListLangVarDeclaration() extends LinkedList[LangVarDeclaration]

class LangStmt_Conditional extends LangStmt {
	protected val _blocks = new ArrayBuffer[BasicBlock]()
	protected var blStart : BasicBlock = null
	protected var blEnd : BasicBlock = null

	override def entryBlock = blStart
	override def exitBlock = blEnd
	override def blocks = _blocks

}

case class LangStmt_If(val condition: LangExp, val blockIfTrue: LangStmt) extends LangStmt_Conditional {
	override def hasSideEffects = condition.hasSideEffects || blockIfTrue.hasSideEffects

	override def resolve(scope : Scope) = {
		condition.resolve(scope)

		if(condition.expType != TypeBool()) {
			throw new CompilationException("Condition should be of type 'boolean', got type "+condition.expType)
		}

		blockIfTrue.resolve(scope)
	}

	override def createBlocks(func : UserFunction) = {
		blStart = func.newBlock
		blEnd = func.newBlock

		val bls_ifTrue = func.compileStatements(List(blockIfTrue), null, blEnd)

		val bls_conds = func.buildConditional(condition, bls_ifTrue.head, blEnd)
		blStart.branchTo(bls_conds.head)

		_blocks += blStart
		_blocks ++= bls_conds
		_blocks ++= bls_ifTrue
		_blocks += blEnd
	}
}

case class LangStmt_IfElse(val condition: LangExp, val blockIfTrue: LangStmt, val blockIfFalse: LangStmt) extends LangStmt_Conditional {
	override def hasSideEffects = condition.hasSideEffects || blockIfTrue.hasSideEffects || blockIfFalse.hasSideEffects

	override def resolve(scope : Scope) = {
		condition.resolve(scope)

		if(condition.expType != TypeBool()) {
			throw new CompilationException("Condition should be of type 'boolean', got type "+condition.expType)
		}

		blockIfTrue.resolve(scope)
		blockIfFalse.resolve(scope)
	}

	override def createBlocks(func : UserFunction) = {
		blStart = func.newBlock
		blEnd = func.newBlock

		val bls_ifTrue = func.compileStatements(List(blockIfTrue), null, blEnd)
		val bls_ifFalse = func.compileStatements(List(blockIfFalse), null, blEnd)

		val bls_conds = func.buildConditional(condition, bls_ifTrue.head, bls_ifFalse.head)
		blStart.branchTo(bls_conds.head)

		_blocks += blStart
		_blocks ++= bls_conds
		_blocks ++= bls_ifTrue
		_blocks ++= bls_ifFalse
		_blocks += blEnd
	}
}

case class LangStmt_While(val condition: LangExp, val blockWhileTrue: LangStmt) extends LangStmt_Conditional {
	// cannot be optimized out, because a while(true) loop can occur
	override def hasSideEffects = true

	override def resolve(scope : Scope) = {
		condition.resolve(scope)

		if(condition.expType != TypeBool()) {
			throw new CompilationException("Condition should be of type 'boolean', got type "+condition.expType)
		}

		blockWhileTrue.resolve(scope)
	}

	override def createBlocks(func : UserFunction) = {
		blStart = func.newBlock
		blEnd = func.newBlock

		//	start
		// 		-> cond
		//	body
		//		-> cond
		//	cond
		//		-> body/end
		//	end

		val bls_whileTrue = func.compileStatements(List(blockWhileTrue), null, null)

		val bls_conds = func.buildConditional(condition, bls_whileTrue.head, blEnd)

		// branch start -> cond
		blStart.branchTo(bls_conds.head)

		// branch body -> cond
		bls_whileTrue(bls_whileTrue.size-1).branchTo(bls_conds.head)

		_blocks += blStart
		_blocks ++= bls_whileTrue
		_blocks ++= bls_conds
		_blocks += blEnd
	}
}

case class LangStmt_Return(returnValue: LangExp) extends LangStmt {
	override def hasSideEffects = true

	override def resolve(scope : Scope) = {
		returnValue.resolve(scope)
		var f = scope.owningFunc

		if(!returnValue.expType.isA(f.funcType.returnType)) {
			throw new CompilationException("Expected return value of type "+f.funcType.returnType
				+" but got type "+returnValue.expType)
		}
	}

	override def createBlocks(func : UserFunction) = {
		_block = func.newBlock

		val (expVal, instrs) = returnValue.compile(_block)
		val instr = new InstrReturn(_block, returnValue.expType, expVal)

		_block.addInstructions(instrs)
		_block.addInstruction(instr)
	}

	override def exitBlock = null
}

case class LangStmt_ReturnNothing() extends LangStmt {
	override def hasSideEffects = true

	override def resolve(scope : Scope) = {
		var f = scope.owningFunc

		if(f.funcType.returnType != TypeVoid()) {
			throw new CompilationException("Expected return value of type "+f.funcType.returnType)
		}
	}

	override def createBlocks(func : UserFunction) = {
		_block = func.newBlock

		val instr = new InstrReturn(_block, TypeVoid(), null)
		_block.addInstruction(instr)
	}

	override def exitBlock = null
}



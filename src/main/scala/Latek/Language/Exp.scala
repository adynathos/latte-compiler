package Latek.Language;
import Latek._
import java.util.LinkedList;
import scala.collection.mutable.ArrayBuffer;
import scala.collection.JavaConversions._;

abstract class LangExp() {
	protected var _expType : Type = null

	def expType : Type = _expType
	def hasSideEffects = false

	def variable : Variable = null
	def func : Function = null

	def resolve(scope : Scope) = {}
	def resolveCallable(scope : Scope) : Unit = {
		throw new CompilationException("Expression "+this+" is not callable.")
	}
	def compile(bl : BasicBlock) : (Value, Seq[Instruction]) = (ValueConstantInt(0), List())
}

case class LangExp_Call(val funcExp: LangExp, val args: ListLangExp) extends LangExp {
	protected var _func : Function = null

	override def hasSideEffects = true
	override def func : Function = _func

	override def resolve(scope : Scope) = {
		funcExp.resolveCallable(scope)
		_func = funcExp.func
		var ftype = _func.funcType

		if(args.size != ftype.argTypes.size) {
			throw new CompilationException("Function "+_func+" requires "+ftype.argTypes.size+
				" arguments but " + args.size +" given.")
		}

		for(((arg_exp, expected_type), idx) <- (args.toList zip ftype.argTypes).zipWithIndex) {
			arg_exp.resolve(scope)

			if(arg_exp.expType != expected_type) {
				throw new CompilationException("Argument "+idx+" to function "+
					_func.name + ": expected type " + expected_type +
					" but got type " + arg_exp.expType)
			}
		}

		_expType = ftype.returnType
	}

	override def compile(bl : BasicBlock) : (Value, Seq[Instruction]) = {
		val instrs = ArrayBuffer[Instruction]()
		val argVals = ArrayBuffer[Value]()

		for(arg_exp <- args.toList) {
			val (expVal, expInstrs) = arg_exp.compile(bl)

			instrs ++= expInstrs
			argVals += expVal
		}

		val instr = new InstrCall(bl, func, argVals)
		instrs += instr

		return (instr.result, instrs)
	}
}

case class LangExp_Array(arrayExp: LangExp, indexExp: LangExp) extends LangExp {
	override def hasSideEffects = arrayExp.hasSideEffects || indexExp.hasSideEffects

	override def resolve(scope : Scope) = {
		throw new CompilationException("Arrays not supported yet.")
	}
}

case class LangExp_Add(val a: LangExp, val op: LangOpAdd, val b: LangExp) extends LangExp {
	override def hasSideEffects = a.hasSideEffects || b.hasSideEffects

	override def resolve(scope : Scope) = {
		a.resolve(scope)
		b.resolve(scope)

		(a.expType, b.expType) match {
			case (TypeInt(), TypeInt()) => {
				_expType = TypeInt()
			}
			case (TypeString(), TypeString()) => {
				if(op == LangOpAdd_Add()) {
					_expType = TypeString()
				}
				else
				{
					throw new CompilationException("Cannot perform operation "+op+" on type "
						+ a.expType + " and " + b.expType + ".")
				}
			}
			case _ => {
				throw new CompilationException("Cannot perform operation "+op+" on type "
					+ a.expType + " and " + b.expType + ".")
			}
		}
	}

	override def compile(bl : BasicBlock) : (Value, Seq[Instruction]) = {
		val regName = bl.nextReg

		val instrs = new ArrayBuffer[Instruction]()
		var result : Value = null

		val (expValA, instrsA) = a.compile(bl)
		val (expValB, instrsB) = b.compile(bl)

		instrs ++= instrsA
		instrs ++= instrsB

		a.expType match {
			case TypeInt() => (expValA, expValB) match {
				// evaluate compile-time constants
				case (ValueConstantInt(constA), ValueConstantInt(constB)) => {
					result = ValueConstantInt(op match {
						case LangOpAdd_Add() => constA + constB
						case LangOpAdd_Sub() => constA - constB
					})
					instrs.clear
				}
				// non constants
				case _ => {
					val instr = new InstrIntAddOp(bl, expValA, expValB, op)

					result = instr.result
					instrs += instr
				}
			}
			case TypeString() => (expValA, expValB) match {
				// evaluate compile-time constants
				case (ValueConstantString(constA), ValueConstantString(constB)) => {
					result = ValueConstantString(constA+constB)
					instrs.clear
				}
				// non constants
				case _ => {
					val instr = new InstrCall(bl, bl.func.program.functionsByName("$internal_string_add"), Seq(expValA, expValB))

					instrs += instr
					result = instr.result
				}
			}
			case _ => {}
		}

		return (result,	instrs)
	}
}

case class LangExp_Mul(val a: LangExp, val op: LangOpMul, val b: LangExp) extends LangExp {
	override def hasSideEffects = a.hasSideEffects || b.hasSideEffects

	override def resolve(scope : Scope) = {
		a.resolve(scope)
		b.resolve(scope)

		(a.expType, b.expType) match {
			case (TypeInt(), TypeInt()) => {
				_expType = TypeInt()
			}
			case _ => {
				throw new CompilationException("Cannot perform operation "+op+" on types "
					+ a.expType + " and " + b.expType + ".")
			}
		}
	}

	override def compile(bl : BasicBlock) : (Value, Seq[Instruction]) = {
		val instrs = new ArrayBuffer[Instruction]()
		var result : Value = null

		val (expValA, instrsA) = a.compile(bl)
		val (expValB, instrsB) = b.compile(bl)

		instrs ++= instrsA
		instrs ++= instrsB

		// detect division by 0
		if(expValB == ValueConstantInt(0) && (op == LangOpMul_Div() || op == LangOpMul_Mod())) {
			throw new CompilationException("Division by 0")
		}

		(expValA, expValB) match {
			// evaluate compile-time constants
			case (ValueConstantInt(constA), ValueConstantInt(constB)) => {
				result = ValueConstantInt(op match {
					case LangOpMul_Mul() => constA * constB
					case LangOpMul_Div() => constA / constB
					case LangOpMul_Mod() => constA % constB
				})
				instrs.clear
			}
			// non constants
			case _ => {
				val instr = new InstrIntMulOp(bl, expValA, expValB, op)

				result = instr.result
				instrs += instr
			}
		}

		return (result,	instrs)
	}
}
case class LangExp_Negation(val a: LangExp) extends LangExp {
	override def hasSideEffects = a.hasSideEffects

	override def resolve(scope : Scope) = {
		a.resolve(scope)

		a.expType match {
			case TypeInt() => {
				_expType = TypeInt()
			}
			case _ => {
				throw new CompilationException("Cannot perform integer negation on type "
					+ a.expType + ".")
			}
		}
	}

	override def compile(bl : BasicBlock) : (Value, Seq[Instruction]) = {
		var result : Value = null
		val instrs =  new ArrayBuffer[Instruction]()

		val (expValA, instrsA) = a.compile(bl)

		expValA match {
			// evaluate compile-time constants
			case ValueConstantInt(constA) => {
				result = ValueConstantInt(-constA)
				instrs.clear
			}
			// non constants
			case _ => {
				val instr = new InstrIntNegation(bl, expValA)

				result = instr.result
				instrs ++= instrsA
				instrs += instr
			}
		}

		return (result, instrs)
	}
}

case class LangExp_Rel(val a: LangExp, val op: LangOpRel, val b: LangExp) extends LangExp {
	override def hasSideEffects = a.hasSideEffects || b.hasSideEffects

	override def resolve(scope : Scope) = {
		a.resolve(scope)
		b.resolve(scope)

		if(!(a.expType.isA(b.expType) || b.expType.isA(a.expType))) {
			throw new CompilationException("Cannot perform operation "+op+" on types "
				+ a.expType + " and " + b.expType + ".")
		}

		a.expType match {
			case TypeInt() => {
				// if type is int all ops are available
			}
			case TypeString() => {
				// if type is int all ops are available
			}
			case _ => {
				if(op != LangRel_EQ() && op != LangRel_NE()) {
					throw new CompilationException("Operation "+op+" incompatible with types "
						+ a.expType + " and " + b.expType + ".")
				}
			}
		}

		_expType = TypeBool()
	}

	override def compile(bl : BasicBlock) : (Value, Seq[Instruction]) = {
		val regName = bl.nextReg

		val instrs = new ArrayBuffer[Instruction]()
		var result : Value = null

		val (expValA, instrsA) = a.compile(bl)
		val (expValB, instrsB) = b.compile(bl)

		instrs ++= instrsA
		instrs ++= instrsB

		a.expType match {
			case TypeInt() => {
				(expValA, expValB) match {
					case (ValueConstantInt(constA), ValueConstantInt(constB)) => {
						result = ValueConstantBool(op match {
							case LangRel_EQ() => constA == constB
							case LangRel_NE() => constA != constB
							case LangRel_GE() => constA >= constB
							case LangRel_GT() => constA >  constB
							case LangRel_LE() => constA <= constB
							case LangRel_LT() => constA <  constB
						})
						instrs.clear
					}
					case _ => {
						val instr = new InstrIntCompare(bl, expValA, expValB, op)

						instrs += instr
						result = instr.result
					}
				}
			}

			case TypeBool() => {
				(expValA, expValB) match {
					case (ValueConstantBool(constA), ValueConstantBool(constB)) => {
						result = ValueConstantBool(op match {
							case LangRel_EQ() => constA == constB
							case LangRel_NE() => constA != constB
						})
						instrs.clear
					}
					case _ => {
						val instr = new InstrBoolCompare(bl, expValA, expValB, op)

						instrs += instr
						result = instr.result
					}
				}
			}
			case TypeString() => {
				val op_id = (op match {
					case LangRel_EQ() => 0
					case LangRel_NE() => 1
					case LangRel_GE() => 2
					case LangRel_GT() => 3
					case LangRel_LE() => 4
					case LangRel_LT() => 5
				})

				val instr = new InstrCall(
					bl,
					bl.func.program.functionsByName("$internal_string_compare"),
					Seq(expValA, expValB, ValueConstantInt(op_id)))

				//println(instr)

				result = instr.result
				instrs += instr
			}
			case _ => {

			}
		}

		return (result,	instrs)
	}
}


case class LangExp_And(a: LangExp, b: LangExp) extends LangExp {
	override def hasSideEffects = a.hasSideEffects || b.hasSideEffects

	override def resolve(scope : Scope) = {
		a.resolve(scope)
		b.resolve(scope)

		(a.expType, b.expType) match {
			case (TypeBool(), TypeBool()) => {
				_expType = TypeBool()
			}
			case _ => {
				throw new CompilationException("Cannot perform logical operation && on type "
					+ a.expType + " and " + b.expType + ".")
			}
		}
	}

	override def compile(bl : BasicBlock) : (Value, Seq[Instruction]) = {
		val instrs = new ArrayBuffer[Instruction]()
		var result : Value = null

		val (expValA, instrsA) = a.compile(bl)
		val (expValB, instrsB) = b.compile(bl)

		expValA match {
			case ValueConstantBool(constA) => {
				// a is const
				if(constA) {
					// true && b = b
					instrs ++= instrsB
					result = expValB
				} else {
					// false && b = false
					instrs.clear
					result = ValueConstantBool(false)
				}
			}
			case _ => expValB match {
				case ValueConstantBool(constB) => {
					if(constB) {
						// a && true = a
						instrs ++= instrsA
						result = expValA
					} else {
						// b && false = false
						result = ValueConstantBool(false)
						if(a.hasSideEffects) {
							instrs ++= instrsA
						}
					}
				}
				case _ => {
					instrs ++= instrsA
					instrs ++= instrsB

					val instr = new InstrBoolAnd(bl, expValA, expValB)
					instrs += instr
					result = instr.result
				}
			}
		}

		return (result,	instrs)
	}
}

case class LangExp_Or(a: LangExp, b: LangExp) extends LangExp {
	override def hasSideEffects = a.hasSideEffects || b.hasSideEffects

	override def resolve(scope : Scope) = {
		a.resolve(scope)
		b.resolve(scope)

		(a.expType, b.expType) match {
			case (TypeBool(), TypeBool()) => {
				_expType = TypeBool()
			}
			case _ => {
				throw new CompilationException("Cannot perform logical operation || on type "
					+ a.expType + " and " + b.expType + ".")
			}
		}
	}

	override def compile(bl : BasicBlock) : (Value, Seq[Instruction]) = {
		val instrs = new ArrayBuffer[Instruction]()
		var result : Value = null

		val (expValA, instrsA) = a.compile(bl)
		val (expValB, instrsB) = b.compile(bl)

		expValA match {
			case ValueConstantBool(constA) => {
				// a is const
				if(constA) {
					// true || b = true
					instrs.clear
					result = ValueConstantBool(true)
				} else {
					// false || b = b
					instrs ++= instrsB
					result = expValB
				}
			}
			case _ => expValB match {
				case ValueConstantBool(constB) => {
					if(constB) {
						// a || true = true
						result = ValueConstantBool(true)
						if(a.hasSideEffects) {
							instrs ++= instrsA
						}
					} else {
						// a || false = a
						instrs ++= instrsA
						result = expValA
					}
				}
				case _ => {
					instrs ++= instrsA
					instrs ++= instrsB

					val instr = new InstrBoolOr(bl, expValA, expValB)
					instrs += instr
					result = instr.result
				}
			}
		}

		return (result,	instrs)
	}
}

case class LangExp_Not(a: LangExp) extends LangExp {
	override def hasSideEffects = a.hasSideEffects

	override def resolve(scope : Scope) = {
		a.resolve(scope)

		a.expType match {
			case TypeBool() => {
				_expType = TypeBool()
			}
			case _ => {
				throw new CompilationException("Cannot perform logical negation on type "
					+ a.expType + ".")
			}
		}
	}

	override def compile(bl : BasicBlock) : (Value, Seq[Instruction]) = {
		val instrs = new ArrayBuffer[Instruction]()
		var result : Value = null

		val (expValA, instrsA) = a.compile(bl)

		expValA match {
			case ValueConstantBool(constA) => {
				instrs.clear
				result = ValueConstantBool(!constA)
			}
			case _ => {
				instrs ++= instrsA

				val instr = new InstrBoolNot(bl, expValA)

				instrs += instr
				result = instr.result
			}
		}

		return (result,	instrs)
	}
}

case class LangExp_New(val className: String) extends LangExp {
	override def hasSideEffects = true

	override def resolve(scope : Scope) = {
		var program = scope.program
		var cls = program.classes getOrElse(className, null)

		if(cls == null) {
			throw new CompilationException("Class "+className+" not declared.")
		}

		_expType = TypeObject(cls)
	}
}

case class LangExp_Member(obj: LangExp, memberName: String) extends LangExp {
	protected var _variable : Variable = null
	protected var _func : Function = null
	var objClass : Class = null

	override def hasSideEffects = obj.hasSideEffects

	override def variable : Variable = _variable
	override def func : Function = _func

	override def resolve(scope : Scope) = {
		obj.resolve(scope)

		obj.expType match {
			case TypeObject(cls) => {
				objClass = cls

				_variable = cls.scope.getVariable(memberName)
				if(variable != null) {
					_expType = variable.varType
				} else {
					throw new CompilationException("No member "+memberName+" in class "+cls+".")
				}
			}
			case _ => {
				throw new CompilationException("Type "+obj.expType+" has no members.")
			}
		}
	}

	override def resolveCallable(scope : Scope) = {
		obj.resolve(scope)

		obj.expType match {
			case TypeObject(cls) => {
				objClass = cls

				_func = cls.scope.getFunction(memberName)
				if(_func != null) {
					_expType = _func.funcType
				} else {
					throw new CompilationException("No method "+memberName+" in class "+cls+".")
				}
			}
			case _ => {
				throw new CompilationException("Type "+obj.expType+" has no methods.")
			}
		}
	}
}

case class LangExp_LitTrue() extends LangExp {
	override def expType = TypeBool()
	override def compile(bl : BasicBlock) = (ValueConstantBool(true), List())
}

case class LangExp_LitFalse() extends LangExp {
	override def expType = TypeBool()
	override def compile(bl : BasicBlock) = (ValueConstantBool(false), List())
}

case class LangExp_LitInt(val value: Integer) extends LangExp {
	override def expType = TypeInt()
	override def compile(bl : BasicBlock) = (ValueConstantInt(value), List())
}

case class LangExp_LitNull(val typeName: String) extends LangExp {
	override def resolve(scope : Scope) = {
		var program = scope.program
		var cls = program.classes getOrElse(typeName, null)

		if(cls == null) {
			throw new CompilationException("Class "+typeName+" not declared.")
		}

		_expType = TypeObject(cls)
	}
}

case class LangExp_LitString(val value: String) extends LangExp {
	override def expType = TypeString()
	override def compile(bl : BasicBlock) = (ValueConstantString(value), List())
}

case class LangExp_This() extends LangExp

case class LangExp_Variable(varName: String) extends LangExp {
	var _variable : Variable = null
	var _func : Function = null

	override def variable = _variable
	override def func = _func

	override def resolve(scope : Scope) = {
		_variable = scope.getVariable(varName)

		if(variable != null) {
			_expType = variable.varType
		} else {
			throw new CompilationException("Variable "+varName+" not declared.")
		}
	}

	override def compile(bl : BasicBlock) : (Value, Seq[Instruction]) = {
		//val instr = new InstrGetLocal(bl, variable.localIndex)
		val instr = new InstrLoad(bl, variable)

		return (instr.result, List(instr))
	}

	override def resolveCallable(scope : Scope) = {
		_func = scope.getFunction(varName)

		_expType = _func.funcType

		if(_func != null) {
			_expType = func.funcType
		} else {
			throw new CompilationException("Function "+varName+" not declared.")
		}
	}
}

abstract class LangOpAdd() {
	override def toString = {
		this match {
			case LangOpAdd_Add() => "+"
			case LangOpAdd_Sub() => "-"
		}
	}
}
case class LangOpAdd_Add() extends LangOpAdd
case class LangOpAdd_Sub() extends LangOpAdd

abstract class LangOpMul() {
	override def toString = {
		this match {
			case LangOpMul_Div() => "/"
			case LangOpMul_Mod() => "%"
			case LangOpMul_Mul() => "*"
		}
	}
}
case class LangOpMul_Div() extends LangOpMul
case class LangOpMul_Mod() extends LangOpMul
case class LangOpMul_Mul() extends LangOpMul

abstract class LangOpRel() {
	override def toString = {
		this match {
			case LangRel_EQ() => "=="
			case LangRel_GE() => ">="
			case LangRel_GT() => ">"
			case LangRel_LE() => "<="
			case LangRel_LT() => "<"
			case LangRel_NE() => "!="
		}
	}
}
case class LangRel_EQ() extends LangOpRel
case class LangRel_GE() extends LangOpRel
case class LangRel_GT() extends LangOpRel
case class LangRel_LE() extends LangOpRel
case class LangRel_LT() extends LangOpRel
case class LangRel_NE() extends LangOpRel

case class ListLangExp() extends LinkedList[LangExp]

abstract class LangLValue() {
	protected var _exp : LangExp = null

	def expType = _exp.expType
	def variable = _exp.variable

	def resolve(scope : Scope) = {
		_exp.resolve(scope)
	}

	def exp : LangExp = _exp
}

case class LangLValue_Array(arrayExp: LangExp, indexExp: LangExp) extends LangLValue
case class LangLValue_Member(objExp: LangExp, memberName: String) extends LangLValue

case class LangLValue_Variable(varName: String) extends LangLValue {
	_exp = LangExp_Variable(varName)
}




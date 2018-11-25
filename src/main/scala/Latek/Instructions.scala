package Latek
import Latek.Language._
import scala.collection.mutable.ArrayBuffer

abstract class Instruction(val block : BasicBlock) {
	protected val retReg = block.nextReg

	def hasResult = true

	def valuesUsed : Set[Value] = Set()
	// def localsUsed : Set[Int] = {
	// 	valuesUsed collect {
	// 		case ValueLocal(idx) => idx
	// 	}
	// }

	def result : Value = {
		if(hasResult) {
			return ValueRegister(retReg)
		} else {
			throw new CompilerException("Instruction has no result")
		}
	}

	// resolves local values and generates string literal globals
	def processValues = {
		//println(valuesUsed)

		for(v <- valuesUsed) {
			v.allocateGlobals(block.func.program)
		}
	}
}

// class InstrGetLocal(bl : BasicBlock, val index : Int) extends Instruction(bl) {
// 	protected var _result : Value = ValueImportedLocal(index)

// 	override def valuesUsed = Set()
// 	override def toString = "; " + _result + " = GET LOCAL ("+index+	")"
// 	override def result = _result

// 	override def processValues = {

// 		super.processValues

// 		//println(bl.localValues)
// 		val loc_val = bl.localValues(index)
// 		loc_val.importRequired = true
// 		_result.setImportedValue(loc_val)

// 		//println("GET LOCAL ("+index+") -> " + result)
// 	}
// }

// class InstrPhi(bl : BasicBlock, val tp : Type, val index : Int) extends Instruction(bl) {
// 	override def toString = {
// 		retReg + " = phi "+tp.llvmName+"" +	bl.successors.map(s => {
// 			"["+s.localValues(index)+", %"+s.label+"]"
// 		}).mkString(",")
// 	}
// }

// class InstrSetLocal(bl : BasicBlock, val index : Int, val value : Value) extends Instruction(bl) {
// 	override def hasResult = false
// 	override def valuesUsed = Set(value)
// 	override def toString = ""

// 	override def processValues = {
// 		//println("SET LOCAL ("+index+") = "+value)
// 		super.processValues

// 		bl.localValues(index) = value
// 		//println(bl.localValues)
// 	}
// }

class InstrBlockLabel(bl : BasicBlock) extends Instruction(bl) {
	override def hasResult = false
	override def toString = bl.label + ":"
}

class IntrEmpty(bl : BasicBlock) extends Instruction(bl) {
	override def hasResult = false
	override def toString = ""
}

class InstrBranch(bl : BasicBlock, val toBlock : BasicBlock) extends Instruction(bl) {
	override def hasResult = false
	override def toString = "br "+toBlock.llvmLabel
}

class InstrBranchConditional(bl : BasicBlock, val condVal : Value, val ifTrue : BasicBlock, val ifFalse : BasicBlock) extends Instruction(bl) {
	override def hasResult = false
	override def toString =
		"br i1 "+condVal+", "+ifTrue.llvmLabel+", "+ifFalse.llvmLabel
}

class InstrReturn(bl : BasicBlock, val retType : Type, val retValue : Value) extends Instruction(bl) {
	override def hasResult = false
	override def toString = {
		if(retType == TypeVoid()) {
			"ret void"
		} else {
			"ret " + retType.llvmName + " " + retValue
		}
	}
}

class InstrCall(bl : BasicBlock, val func : Function, val argVals : Seq[Value]) extends Instruction(bl) {
	override def valuesUsed = Set() ++ argVals

	override def toString = {
		val argTypes = func.funcType.argTypes
		var line = ("call "+func.returnType.llvmName+" "+func.llvmName+"("
			+ (argTypes zip argVals).map({case (tp, v) => tp.llvmName + " " + v}).mkString(",")
			+ ")")

		if(func.returnType != TypeVoid()) {
			line = retReg + " = " + line
		}

		line
	}
}

class InstrAlloca(bl : BasicBlock, val regName : String, val varType : Type) extends Instruction(bl) {
	override val retReg = regName
	override def toString = retReg + " = alloca " + varType.llvmName
}

class InstrLoad(bl : BasicBlock, val from : Variable) extends Instruction(bl) {
	override def toString = retReg + " = load " + from.llvmPtr
}

class InstrStore(bl : BasicBlock, val to : Variable, val value : Value) extends Instruction(bl) {
	override def hasResult = false
	override def valuesUsed = Set(value)
	override def toString = "store " + to.varType.llvmName + " " + value + ", " + to.llvmPtr
}

class InstrIntAddOp(bl : BasicBlock, val a : Value, val b : Value, val op : LangOpAdd) extends Instruction(bl) {
	override def valuesUsed = Set(a, b)
	override def toString : String = {
		val opInstr = op match {
			case LangOpAdd_Add() => "add nsw"
			case LangOpAdd_Sub() => "sub nsw"
		}

		return retReg + " = " + opInstr + " i32 " + a + ", " + b
	}
}

class InstrIntMulOp(bl : BasicBlock, val a : Value, val b : Value, val op : LangOpMul) extends Instruction(bl) {
	override def valuesUsed = Set(a, b)
	override def toString : String = {
		val opInstr = op match {
			case LangOpMul_Mul() => "mul"
			case LangOpMul_Div() => "sdiv"
			case LangOpMul_Mod() => "srem"
		}

		return retReg + " = " + opInstr + " i32 " + a + ", " + b
	}
}

class InstrIntNegation(bl : BasicBlock, val a : Value) extends Instruction(bl) {
	override def valuesUsed = Set(a)
	override def toString = retReg + " = sub i32 0, "+a
}

class InstrBoolAnd(bl : BasicBlock, val a : Value, val b : Value) extends Instruction(bl) {
	override def valuesUsed = Set(a, b)
	override def toString = retReg + " = and i1 " + a + ", " + b
}

class InstrBoolOr(bl : BasicBlock, val a : Value, val b : Value) extends Instruction(bl) {
	override def valuesUsed = Set(a, b)
	override def toString = retReg + " = or i1 " + a + ", " + b
}

class InstrBoolNot(bl : BasicBlock, val a : Value) extends Instruction(bl) {
	override def valuesUsed = Set(a)
	override def toString = retReg + " = add i1 " + a + ", 1"
}

class InstrIntCompare(bl : BasicBlock, val a : Value, val b : Value, val op : LangOpRel) extends Instruction(bl) {
	override def valuesUsed = Set(a, b)
	override def toString : String = {
		val instr_cond = op match {
			case LangRel_EQ() => "eq"
			case LangRel_NE() => "ne"
			case LangRel_GE() => "sge"
			case LangRel_GT() => "sgt"
			case LangRel_LE() => "sle"
			case LangRel_LT() => "slt"
		}

		retReg + " = icmp "+instr_cond+" i32 "+a+", "+b
	}
}

class InstrBoolCompare(bl : BasicBlock, val a : Value, val b : Value, val op : LangOpRel) extends Instruction(bl) {
	override def valuesUsed = Set(a, b)
	override def toString : String = {
		val instr_cond = op match {
			case LangRel_EQ() => "eq"
			case LangRel_NE() => "ne"
			case LangRel_GE() => "sge"
			case LangRel_GT() => "sgt"
			case LangRel_LE() => "sle"
			case LangRel_LT() => "slt"
		}

		retReg + " = icmp "+instr_cond+" i1 "+a+", "+b
	}
}

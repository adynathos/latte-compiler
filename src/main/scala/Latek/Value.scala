package Latek
import Latek.Language._

abstract class Value() {
	def allocateGlobals(prog : Program) = {}
	def importRequired = false
	def importRequired_=(x : Boolean) = {}
	def setImportedValue(v : Value) = {}
}

case class ValueConstantInt(val value : Integer) extends Value {
	override def toString = value.toString
}

case class ValueConstantBool(val value : Boolean) extends Value {
	override def toString = if(value) "1" else "0"
}

case class ValueConstantString(val value : String) extends Value {
	private var program : Program = null

	override def allocateGlobals(prog : Program) = {
		program = prog
	}

	override def toString =
		if(program != null) program.stringLiteralName(value) else "'"+value+"'"
}

case class ValueRegister(val regName : String) extends Value {
	override def toString = regName
}

case class ValueImportedLocal(val localIndex : Int) extends Value {
	protected var _importRequired = false
	protected var resolvedValue : Value = null

	override def toString = if(resolvedValue != null) resolvedValue.toString else "NO RES VAL "+localIndex

	override def importRequired = _importRequired
	override def importRequired_=(x : Boolean) = {_importRequired = x}
	override def setImportedValue(v : Value) = {resolvedValue = v}
}


package Latek;
import Latek.Language._;

class Type(val name : String) {
	def instantiable = true;
	def isA(other : Type) : Boolean = {
		return this == other;
	}
	def llvmName : String = "INVALID";
	def llvmPtr : String = llvmName+"*";

	def baseExp : LangExp = null;

	override def toString = "'"+name+"'"
}

case class TypeInt() extends Type("int") {
	override def llvmName = "i32";
	override def baseExp = LangExp_LitInt(0);
}

case class TypeBool() extends Type("boolean") {
	override def llvmName = "i1";
	override def baseExp = LangExp_LitFalse();
}

case class TypeString() extends Type("string") {
	override def llvmName = "i8*";
	override def baseExp = LangExp_LitString("");
}

case class TypeObject(val cls : Class) extends Type(cls.name) {
	override def isA(other : Type) : Boolean = {
		return other match {
			case TypeObject(other_cls) => {
				cls.isSubclass(other_cls)
			}
			case _ => {
				false
			}
		}
	}

	override def baseExp = LangExp_LitNull(cls.name)
}

case class TypeVoid() extends Type("void") {
	override def instantiable = false
	override def llvmName = "void"
}

case class TypeFunction(val returnType : Type, val argTypes : List[Type])
extends Type(returnType.name + "(" + argTypes.map((tp) => tp.name).mkString(",") + ")") {
	override def instantiable = false
}


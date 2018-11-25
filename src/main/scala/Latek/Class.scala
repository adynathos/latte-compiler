package Latek;
import Latek.Language._;
import scala.collection.JavaConversions._
import scala.collection.mutable.{Map, Set, ArrayBuffer};

class Class(val classDef : LangClassDef, val program : Program) {
	val name = classDef.className;
	val scope = new Scope(program.globalScope);
	val fields = ArrayBuffer[Variable]();
	val methods = ArrayBuffer[Function]();
	var _superclass : Class = null;

	override def toString() = "class " + name;

	def superclass = _superclass;

	def isSubclass(other : Class) : Boolean = {
		if(other == this) {
			return true;
		} else if(superclass == null) {
			return false;
		} else {
			return superclass.isSubclass(other);
		}
	}

	def display() = {
		println("class "+name+" extends "+_superclass);

		for(f <- fields) {
			println("	"+f.name+"	:	"+f.varType)
		}

		for(m <- methods) {
			println("	"+m.name + "	:	"+m.funcType)
		}
	}

	def scanMembers() = {
		// scan superclass
		if(classDef.superclassName != "") {
			_superclass = program.classes.getOrElse(classDef.superclassName, null);

			if(_superclass == null) {
				throw new CompilationException("Superclass " + classDef.superclassName + " of class "
					+ name + " not declared.");
			}
		}

		// scan members
		for(memberDef <- classDef.members.toList) {
			memberDef match {
				case LangClassDefMember_Field(typeName, name) => {
					val v = new Variable(name, program.getType(typeName));
					scope.addVariable(v.name, v);
					fields += v;
				}

				case LangClassDefMember_Method(funcDef) => {
					val m = new Method(program, funcDef, this);

					methods += m;
					program.functions += m;

					scope.addFunction(m.name, m);
				}
			}
		}
	}
}

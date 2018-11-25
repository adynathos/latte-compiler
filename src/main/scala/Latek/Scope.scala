package Latek;
import Latek.Language._;
import scala.collection.JavaConversions._
import scala.collection.mutable.{Map, Set, ArrayBuffer};

class Scope(val parentScope: Scope, private val _function : UserFunction = null, private val _program : Program = null) {
	val variables : Map[String, Variable] = Map[String, Variable]();
	val functions : Map[String, Function] = Map[String, Function]();

	// return the function owning this scope
	def owningFunc : UserFunction = {
		if(_function != null) {
			return _function;
		}

		if(parentScope != null) {
			return parentScope.owningFunc;
		}

		return null;
	}

	def program : Program = {
		if(_program != null) {
			return _program;
		} else if (parentScope != null) {
			return parentScope.program;
		} else {
			return null;
		}
	}

	def getVariable(name : String) : Variable = {
		if(variables contains name) {
			return variables(name);
		} else {
			if(parentScope != null) {
				return parentScope.getVariable(name);
			} else {
				throw new CompilationException("Variable " + name + " not declared.");
			}
		}
	}

	def getFunction(name : String, excludeGlobal : Boolean = false) : Function = {
		if(functions contains name) {
			return functions(name);
		} else {
			if(parentScope != null && !(excludeGlobal && parentScope == program.globalScope)) {
				return parentScope.getFunction(name);
			} else {
				throw new CompilationException("Function " + name + " not declared.");
			}
		}
	}

	def addVariable(varName : String, v : Variable) = {
		if(variables contains varName) {
			throw new CompilationException("Variable " + varName + " already declared.");
		} else {
			variables += (varName -> v);
		}
	}

	def addFunction(funcName : String, f : Function) = {
		if(functions contains funcName) {
			throw new CompilationException("Function " + funcName + " already declared.");
		} else {
			functions += (funcName -> f);
		}
	}
}

package Latek;

class CompilerException(msg: String) extends RuntimeException(msg)

class ParseException(msg: String) extends CompilerException(msg)

class CompilationException(msg: String) extends CompilerException(msg) {
	var _func : Function = null;
	def setFunc(f : Function) = {_func = f}
	override def getMessage : String = {
		var msg = super.getMessage

		if(_func != null) {
			msg = "In function "+_func.name+": \n"+msg;
		}

		return msg;
	}
}

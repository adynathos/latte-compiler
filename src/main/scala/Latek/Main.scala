package Latek
import Latek.Language._
import java.io._
import scala.sys.process._

object Main {

	def runCompiler(in_file_path : File) {
		try {
			val in_file = new BufferedReader(new FileReader(in_file_path))

			val pattern = """(.*)[.]([^.]*)""".r
			val in_file_base_name = in_file_path.getName match { case pattern(fn, ext) => fn }

			val out_file_path = new File(in_file_path.getParent, in_file_base_name + ".ll")
			val compiled_file_path = new File(in_file_path.getParent, in_file_base_name + ".bc")

			val lexer = new Yylex(in_file)
			val par = new parser(lexer)

			try {
				val parse_tree = par.pLangProgram()

				val prog = new Program(parse_tree)

				val out_file = new BufferedWriter(new FileWriter(out_file_path))
				prog.writeTo(out_file)
				out_file.close()

			} catch {
				case ex: ParseException => {
					System.err.println("ERROR")
					System.err.println("Parse error at line " + lexer.line_num() + ", near \"" + lexer.buff() + "\":")
					System.err.println(ex.getMessage())
					System.exit(1)
				}

				case ex: CompilationException => {
					System.err.println("ERROR")
					System.err.println("Compilation exception:")
					System.err.println(ex.getMessage())
					System.exit(1)
				}
			}

			in_file.close()


			val llvm_ret = (
				Seq("llvm-as", out_file_path.toString, "-o", "tmp.bc")
				### Seq("llvm-link", "-o", compiled_file_path.toString, "tmp.bc", "lib/runtime.bc")
				### Seq("rm", "tmp.bc")
			).run.exitValue

			if(llvm_ret != 0) {
				System.err.println("ERROR")
				System.err.println("llvm error")
				System.exit(1)
			} else {
				System.err.println("OK")
			}

		} catch {
			case ex: FileNotFoundException => {
				System.err.println("ERROR")
				System.err.println("Missing file exception: " + ex.getMessage)
				System.exit(1)
			}
			case ex: IOException => {
				System.err.println("ERROR")
				System.err.println("IO Exception: " + ex.getMessage)
				System.exit(1)
			}
		} finally {
			////
		}
	}

	def main(args : Array[String]) {
		if(args.length == 0) {
			System.err.println("ERROR")
			System.err.println("No input file")
			System.exit(1)
		} else {
			runCompiler(new File(args(0)))
		}
	}
}

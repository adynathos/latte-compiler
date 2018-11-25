package Latek;
import Latek.Language._;
import java.io.Writer;
import scala.collection.JavaConversions._
import scala.collection.mutable.{Map, Set, ArrayBuffer};

class Program(val langProgram : LangProgram) {
	val globalScope = new Scope(null, null, this)
	val types = Map[String, Type]()
	val classes = Map[String, Class]()
	val functions = Set[Function]()
	val functionsByName = Map[String, Function]()
	val stringLiterals = Map[String, Int]("" -> 0)
	private var stringLiteralIndex = 1

	def stringLiteralName(lit : String) : String = {
		if(!stringLiterals.contains(lit)) {
			stringLiterals += (lit -> stringLiteralIndex)
			stringLiteralIndex += 1
		}

		val lit_name = "@.str_"+stringLiterals(lit)

		val text = ("getelementptr inbounds (["+(lit.size+1)+" x i8]* " +
			lit_name+", i32 0, i32 0)");

		return text
	}

	def getType(typeName : String) : Type = {
		types.get(typeName) match {
			case None => {
				throw new CompilationException("Type " + typeName + " is not declared.");
				return null;
			}
			case Some(tp) => {
				return tp;
			}
		}
	}

	def addType(tp : Type) {
		if(types contains tp.name) {
			throw new CompilationException("Type " + tp.name + " already declared.");
		}

		types += (tp.name -> tp);
	}

	def addClass(cls : Class) {
		addType(new TypeObject(cls));

		classes += (cls.name -> cls);
	}

	def scanTypes() {
		for(topdef <- langProgram.topDefs.toList) {
			topdef match {
				case LangTopDef_Class(classDef) => {
					val cls = new Class(classDef, this);
					addClass(cls);
				}
				case _ => ;
			}
		}
	}

	def scanFunctions() {
		for(topdef <- langProgram.topDefs.toList) {
			topdef match {
				case LangTopDef_Function(fd) => {
					val func = new UserFunction(this, fd);

					functions += (func);
					functionsByName += (func.name -> func)

					globalScope.addFunction(func.name, func);
				}
				case _ => {
				}
			}
		}

		for((name, cls) <- classes) {
			cls.scanMembers();
			cls.display();
		}
	}

	def addNativeFunctions() {
		val natives = List(
			new NativeFunction(this, "printInt", TypeFunction(TypeVoid(), List(TypeInt()))),
			new NativeFunction(this, "printString", TypeFunction(TypeVoid(), List(TypeString()))),
			new NativeFunction(this, "error", TypeFunction(TypeVoid(), List())),
			new NativeFunction(this, "readInt", TypeFunction(TypeInt(), List())),
			new NativeFunction(this, "readString", TypeFunction(TypeString(), List())),
			new NativeFunction(this, "$internal_string_add", TypeFunction(TypeString(), List(TypeString(), TypeString()))),
			new NativeFunction(this, "$internal_string_compare", TypeFunction(TypeBool(), List(TypeString(), TypeString(), TypeInt()))))

		for(nf <- natives) {
			functions += nf
			functionsByName += (nf.name -> nf)
			globalScope.addFunction(nf.name, nf)
		}
	}

	def resolve() {
		for(f <- functions) {
			f.resolve;
		}
	}

	def compile() {
		for(f <- functions) {
			f.compile;
		}
	}

	def writeTo(out_file : Writer) {
		// write string literals
		for((str, idx) <- stringLiterals) {
			out_file.write(
				"@.str_" + idx + " = private unnamed_addr constant " +
				"[" + (str.size+1) + " x i8] c\"" + str + "\\00\", align 1\n")

//			@.str = private unnamed_addr constant [4 x i8] c"%d\0A\00", align 1
		}

		// write declarations
		for(f <- functions) {
			if(f.isExternal) {
				out_file.write(f.headerDeclare + "\n");
			}
		}

		// write function body
		for(f <- functions) {
			var code = f.code.mkString("\n");

			out_file.write(code);
		}
	}

	addType(new TypeInt());
	addType(new TypeBool());
	addType(new TypeString());
	addType(new TypeVoid());

	scanTypes();

	scanFunctions();
	addNativeFunctions();

	resolve();

	compile();


}





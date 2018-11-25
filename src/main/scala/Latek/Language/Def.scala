package Latek.Language;
import java.util.LinkedList;

case class LangProgram(val topDefs: ListLangTopDef)
case class ListLangTopDef() extends LinkedList[LangTopDef]

class LangTopDef()
case class LangTopDef_Class(classDef: LangClassDef) extends LangTopDef
case class LangTopDef_Function(funcDef: LangFuncDef) extends LangTopDef

class LangClassDef(val className: String, val superclassName: String, val members: ListLangClassDefMember)

case class LangClassDef_BaseClass(_className: String, _members: ListLangClassDefMember)
	extends LangClassDef(_className, "", _members)

case class LangClassDef_SubClass(_className: String, _superclassName: String, _members: ListLangClassDefMember)
	extends LangClassDef(_className, _superclassName, _members)

case class ListLangClassDefMember() extends LinkedList[LangClassDefMember]

class LangClassDefMember()
case class LangClassDefMember_Field(val typeName: String, val name: String) extends LangClassDefMember
case class LangClassDefMember_Method(val funcDef: LangFuncDef) extends LangClassDefMember

case class LangFuncDef(val returnTypeName: String, val name : String, val arguments: ListLangArgument, val body: LangFuncBlock)
case class LangArgument(val typeName: String, val name: String)
case class ListLangArgument() extends LinkedList[LangArgument]

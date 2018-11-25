package Latek
import Latek.Language._
import scala.collection.mutable.{Set, ArrayBuffer}


class BasicBlock(val func : UserFunction, val index : Int) {
	protected var _mergedTo : BasicBlock = null
	protected val _instructions = new ArrayBuffer[Instruction]()
	protected val _label = "b"+index
	protected var _predecessors = Set[BasicBlock]()
	protected var _successors = Set[BasicBlock]()
	protected var _isReturnStatement = false

	protected var regIndex = 0

	var visible = false

	// val _localValues = new ArrayBuffer[Value]()
	// //val localsToImport = Set[Int]()

	// for(idx <- 0 until func.locals.size) {
	// 	localValues += ValueImportedLocal(idx)
	// }

	override def toString = "block("+index+") <"+label+">"
	def label : String =
		if(_mergedTo == null) _label else _mergedTo.label

	def instructions : ArrayBuffer[Instruction] =
		if(_mergedTo == null) _instructions else _mergedTo.instructions

	def predecessors : Set[BasicBlock] =
		if(_mergedTo == null) _predecessors else _mergedTo.predecessors

	def successors : Set[BasicBlock] =
		if(_mergedTo == null) _successors else _mergedTo.successors

	// def localValues : ArrayBuffer[Value] =
	// 	if(_mergedTo == null) _localValues else _mergedTo.localValues

	def isReturnStatement : Boolean = {
		if(_mergedTo != null) {
			return _mergedTo.isReturnStatement
		}

		for(instr <- instructions) {
			if(instr.isInstanceOf[InstrReturn]) {
				return true
			}
		}
		return false
	}

	// def localsUsed : Set[Int] = {
	// 	if(_mergedTo != null) {
	// 		return _mergedTo.localsUsed
	// 	}

	// 	val loc_used = Set[Int]()

	// 	for(instr <- instructions) {
	// 		loc_used ++= instr.localsUsed
	// 	}

	// 	return loc_used
	// }

	def branchTo(b : BasicBlock) = {
		//println(this + " -> " + b)

		successors += b
		b.predecessors += this
	}

	def addInstruction(line : Instruction) = {
		_instructions += line
	}

	def addInstructions(line_seq : Seq[ Instruction]) = {
		_instructions ++= line_seq
	}

	def mergeTo(bl : BasicBlock) = {
		//println(this + " merging to "+bl)

		_mergedTo = bl
		_mergedTo.instructions ++= _instructions
		_mergedTo.successors -= this
		_mergedTo.successors ++= _successors
		_mergedTo.predecessors ++= (_predecessors - bl)
	}

	def nextReg : String = {
		regIndex += 1
		return "%b"+index+"_"+regIndex
	}

	def llvmLabel : String = "label %"+label

	def collectControlGraph : Seq[BasicBlock] = {
		if(!visible) {
			visible = true
			val allBlocks = new ArrayBuffer[BasicBlock]()

			// if it contains a return statement, ignore successors
			if(isReturnStatement) {
				successors.clear
			}

			//println("discover "+this+": "+successors + " " + (if(isReturnStatement) "RETURN" else ""))

			// merge to single predecessor
			if(predecessors.size == 1) {
				val prev = predecessors.head
				if(prev.successors.size == 1 && prev.visible) {
					mergeTo(prev)
				}
			}

			// merge to empty predecessors
			for(prev_bl <- _predecessors) {
				if(_mergedTo == null && prev_bl.visible && prev_bl.instructions.size == 0 && prev_bl.successors.size == 1) {
					mergeTo(prev_bl)
				}
			}


			if(_mergedTo == null) {
				allBlocks += this
			}

			for(next_bl <- _successors) {
				allBlocks ++= next_bl.collectControlGraph
			}

			return allBlocks
		}

		return List()
	}

	def processValues = {
		//println("RESOLVE LOCAL VALUES "+this)
		for(instr <- instructions) {
			//println("VALUES "+localValues)

			instr.processValues
		}
	}

	def generateConnections = {
		//println("GEN CONNECTIONS "+this)

		//println(instructionsGEN)

		// value imports
		// for(loc_idx <- 0 until localValues.size) {
		// 	var loc_val = localValues(loc_idx)

		// 	if(loc_val.importRequired) {
		// 		val succ_vals = successors.map(s => s.localValues(loc_idx))

		// 		println("IMPORT "+loc_idx)

		// 		// are they the same?
		// 		if(succ_vals.forall( sv => sv == succ_vals.head ) && !succ_vals.head.isInstanceOf[ValueImportedLocal]) {
		// 			println("	val = "+succ_vals.head)
		// 			loc_val.setImportedValue(succ_vals.head)
		// 		} else {
		// 			// generate phi
		// 			println("phi")
		// 			val phi_instr = new InstrPhi(this, func.locals(loc_idx).varType, loc_idx)
		// 			_instructions.prepend(phi_instr)
		// 			loc_val.setImportedValue(phi_instr.result)
		// 		}
		// 	}
		// }

		//label
		_instructions.prepend(new InstrBlockLabel(this))

		//if this has no successors, it needs a return statement
		if(successors.size == 0 && !isReturnStatement) {
			if(func.returnType == TypeVoid()) {
				_instructions.append(new InstrReturn(this, TypeVoid(), null))
			} else {
				throw new CompilationException("Control reaches end of function without returning")
			}
		}

		if(successors.size == 1) {
			//println(this +" autobranch to "+successors)
			addInstruction(new InstrBranch(this, successors.head))
		}
	}
}

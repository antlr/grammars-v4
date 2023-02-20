// Copyright 2011 Google Inc. All Rights Reserved.
// Copyright 1996 John Maloney and Mario Wolczko
//
// This file is part of GNU Smalltalk.
//
// GNU Smalltalk is free software; you can redistribute it and/or modify it
// under the terms of the GNU General Public License as published by the Free
// Software Foundation; either version 2, or (at your option) any later version.
//
// GNU Smalltalk is distributed in the hope that it will be useful, but WITHOUT
// ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
// FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
// details.
//
// You should have received a copy of the GNU General Public License along with
// GNU Smalltalk; see the file COPYING.  If not, write to the Free Software
// Foundation, 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
//
// Translated first from Smalltalk to JavaScript, and finally to
// Dart by Google 2008-2010.
//
// Translated to Wren by Bob Nystrom 2014.

// A Wren implementation of the DeltaBlue constraint-solving
// algorithm, as described in:
//
// "The DeltaBlue Algorithm: An Incremental Constraint Hierarchy Solver"
//   Bjorn N. Freeman-Benson and John Maloney
//   January 1990 Communications of the ACM,
//   also available as University of Washington TR 89-08-06.
//
// Beware: this benchmark is written in a grotesque style where
// the constraint model is built by side-effects from constructors.
// I've kept it this way to avoid deviating too much from the original
// implementation.

// Strengths are used to measure the relative importance of constraints.
// New strengths may be inserted in the strength hierarchy without
// disrupting current constraints.  Strengths cannot be created outside
// this class, so == can be used for value comparison.
class Strength {
  construct new(value, name) {
    _value = value
    _name = name
  }

  value { _value }
  name { _name }

  nextWeaker { ORDERED[_value] }

  static stronger(s1, s2) { s1.value < s2.value }
  static weaker(s1, s2) { s1.value > s2.value }
  static weakest(s1, s2) { Strength.weaker(s1, s2) ? s1 : s2 }
  static strongest(s1, s2) { Strength.stronger(s1, s2) ? s1 : s2 }
}

// Compile time computed constants.
var REQUIRED        = Strength.new(0, "required")
var STRONG_REFERRED = Strength.new(1, "strongPreferred")
var PREFERRED       = Strength.new(2, "preferred")
var STRONG_DEFAULT  = Strength.new(3, "strongDefault")
var NORMAL          = Strength.new(4, "normal")
var WEAK_DEFAULT    = Strength.new(5, "weakDefault")
var WEAKEST         = Strength.new(6, "weakest")

var ORDERED = [
  WEAKEST, WEAK_DEFAULT, NORMAL, STRONG_DEFAULT, PREFERRED, STRONG_REFERRED
]

var ThePlanner

class Constraint {
  construct new(strength) {
    _strength = strength
  }

  strength { _strength }

  // Activate this constraint and attempt to satisfy it.
  addConstraint() {
    addToGraph()
    ThePlanner.incrementalAdd(this)
  }

  // Attempt to find a way to enforce this constraint. If successful,
  // record the solution, perhaps modifying the current dataflow
  // graph. Answer the constraint that this constraint overrides, if
  // there is one, or nil, if there isn't.
  // Assume: I am not already satisfied.
  satisfy(mark) {
    chooseMethod(mark)
    if (!isSatisfied) {
      if (_strength == REQUIRED) {
        System.print("Could not satisfy a required constraint!")
      }
      return null
    }

    markInputs(mark)
    var out = output
    var overridden = out.determinedBy
    if (overridden != null) overridden.markUnsatisfied()
    out.determinedBy = this
    if (!ThePlanner.addPropagate(this, mark)) System.print("Cycle encountered")
    out.mark = mark
    return overridden
  }

  destroyConstraint() {
    if (isSatisfied) ThePlanner.incrementalRemove(this)
    removeFromGraph()
  }

  // Normal constraints are not input constraints.  An input constraint
  // is one that depends on external state, such as the mouse, the
  // keybord, a clock, or some arbitraty piece of imperative code.
  isInput { false }
}

// Abstract superclass for constraints having a single possible output variable.
class UnaryConstraint is Constraint {
  construct new(myOutput, strength) {
    super(strength)
    _satisfied = false
    _myOutput = myOutput
    addConstraint()
  }

  // Adds this constraint to the constraint graph.
  addToGraph() {
    _myOutput.addConstraint(this)
    _satisfied = false
  }

  // Decides if this constraint can be satisfied and records that decision.
  chooseMethod(mark) {
    _satisfied = (_myOutput.mark != mark) &&
        Strength.stronger(strength, _myOutput.walkStrength)
  }

  // Returns true if this constraint is satisfied in the current solution.
  isSatisfied { _satisfied }

  markInputs(mark) {
    // has no inputs.
  }

  // Returns the current output variable.
  output { _myOutput }

  // Calculate the walkabout strength, the stay flag, and, if it is
  // 'stay', the value for the current output of this constraint. Assume
  // this constraint is satisfied.
  recalculate() {
    _myOutput.walkStrength = strength
    _myOutput.stay = !isInput
    if (_myOutput.stay) execute() // Stay optimization.
  }

  // Records that this constraint is unsatisfied.
  markUnsatisfied() {
    _satisfied = false
  }

  inputsKnown(mark) { true }

  removeFromGraph() {
    if (_myOutput != null) _myOutput.removeConstraint(this)
    _satisfied = false
  }
}

// Variables that should, with some level of preference, stay the same.
// Planners may exploit the fact that instances, if satisfied, will not
// change their output during plan execution.  This is called "stay
// optimization".
class StayConstraint is UnaryConstraint {
  construct new(variable, strength) {
    super(variable, strength)
  }

  execute() {
    // Stay constraints do nothing.
  }
}

// A unary input constraint used to mark a variable that the client
// wishes to change.
class EditConstraint is UnaryConstraint {
  construct new(variable, strength) {
    super(variable, strength)
  }

  // Edits indicate that a variable is to be changed by imperative code.
  isInput { true }

  execute() {
    // Edit constraints do nothing.
  }
}

// Directions.
var NONE = 1
var FORWARD = 2
var BACKWARD = 0

// Abstract superclass for constraints having two possible output
// variables.
class BinaryConstraint is Constraint {
  construct new(v1, v2, strength) {
    super(strength)
    _v1 = v1
    _v2 = v2
    _direction = NONE
    addConstraint()
  }

  direction { _direction }
  v1 { _v1 }
  v2 { _v2 }

  // Decides if this constraint can be satisfied and which way it
  // should flow based on the relative strength of the variables related,
  // and record that decision.
  chooseMethod(mark) {
    if (_v1.mark == mark) {
      if (_v2.mark != mark &&
          Strength.stronger(strength, _v2.walkStrength)) {
        _direction = FORWARD
      } else {
        _direction = NONE
      }
    }

    if (_v2.mark == mark) {
      if (_v1.mark != mark &&
          Strength.stronger(strength, _v1.walkStrength)) {
        _direction = BACKWARD
      } else {
        _direction = NONE
      }
    }

    if (Strength.weaker(_v1.walkStrength, _v2.walkStrength)) {
      if (Strength.stronger(strength, _v1.walkStrength)) {
        _direction = BACKWARD
      } else {
        _direction = NONE
      }
    } else {
      if (Strength.stronger(strength, _v2.walkStrength)) {
        _direction = FORWARD
      } else {
        _direction = BACKWARD
      }
    }
  }

  // Add this constraint to the constraint graph.
  addToGraph() {
    _v1.addConstraint(this)
    _v2.addConstraint(this)
    _direction = NONE
  }

  // Answer true if this constraint is satisfied in the current solution.
  isSatisfied { _direction != NONE }

  // Mark the input variable with the given mark.
  markInputs(mark) {
    input.mark = mark
  }

  // Returns the current input variable
  input { _direction == FORWARD ? _v1 : _v2 }

  // Returns the current output variable.
  output { _direction == FORWARD ? _v2 : _v1 }

  // Calculate the walkabout strength, the stay flag, and, if it is
  // 'stay', the value for the current output of this
  // constraint. Assume this constraint is satisfied.
  recalculate() {
    var ihn = input
    var out = output
    out.walkStrength = Strength.weakest(strength, ihn.walkStrength)
    out.stay = ihn.stay
    if (out.stay) execute()
  }

  // Record the fact that this constraint is unsatisfied.
  markUnsatisfied() {
    _direction = NONE
  }

  inputsKnown(mark) {
    var i = input
    return i.mark == mark || i.stay || i.determinedBy == null
  }

  removeFromGraph() {
    if (_v1 != null) _v1.removeConstraint(this)
    if (_v2 != null) _v2.removeConstraint(this)
    _direction = NONE
  }
}

// Relates two variables by the linear scaling relationship: "v2 =
// (v1 * scale) + offset". Either v1 or v2 may be changed to maintain
// this relationship but the scale factor and offset are considered
// read-only.
class ScaleConstraint is BinaryConstraint {
  construct new(src, scale, offset, dest, strength) {
    _scale = scale
    _offset = offset
    super(src, dest, strength)
  }

  // Adds this constraint to the constraint graph.
  addToGraph() {
    super()
    _scale.addConstraint(this)
    _offset.addConstraint(this)
  }

  removeFromGraph() {
    super()
    if (_scale != null) _scale.removeConstraint(this)
    if (_offset != null) _offset.removeConstraint(this)
  }

  markInputs(mark) {
    super.markInputs(mark)
    _scale.mark = _offset.mark = mark
  }

  // Enforce this constraint. Assume that it is satisfied.
  execute() {
    if (direction == FORWARD) {
      v2.value = v1.value * _scale.value + _offset.value
    } else {
      // TODO: Is this the same semantics as ~/?
      v1.value = ((v2.value - _offset.value) / _scale.value).floor
    }
  }

  // Calculate the walkabout strength, the stay flag, and, if it is
  // 'stay', the value for the current output of this constraint. Assume
  // this constraint is satisfied.
  recalculate() {
    var ihn = input
    var out = output
    out.walkStrength = Strength.weakest(strength, ihn.walkStrength)
    out.stay = ihn.stay && _scale.stay && _offset.stay
    if (out.stay) execute()
  }
}

// Constrains two variables to have the same value.
class EqualityConstraint is BinaryConstraint {
  construct new(v1, v2, strength) {
    super(v1, v2, strength)
  }

  // Enforce this constraint. Assume that it is satisfied.
  execute() {
    output.value = input.value
  }
}

// A constrained variable. In addition to its value, it maintain the
// structure of the constraint graph, the current dataflow graph, and
// various parameters of interest to the DeltaBlue incremental
// constraint solver.
class Variable {
  construct new(name, value) {
    _constraints = []
    _determinedBy = null
    _mark = 0
    _walkStrength = WEAKEST
    _stay = true
    _name = name
    _value = value
  }

  constraints { _constraints }
  determinedBy { _determinedBy }
  determinedBy=(value) { _determinedBy = value }
  mark { _mark }
  mark=(value) { _mark = value }
  walkStrength { _walkStrength }
  walkStrength=(value) { _walkStrength = value }
  stay { _stay }
  stay=(value) { _stay = value }
  value { _value }
  value=(newValue) { _value = newValue }

  // Add the given constraint to the set of all constraints that refer
  // this variable.
  addConstraint(constraint) {
    _constraints.add(constraint)
  }

  // Removes all traces of c from this variable.
  removeConstraint(constraint) {
    _constraints = _constraints.where { |c| c != constraint }
    if (_determinedBy == constraint) _determinedBy = null
  }
}

// A Plan is an ordered list of constraints to be executed in sequence
// to resatisfy all currently satisfiable constraints in the face of
// one or more changing inputs.
class Plan {
  construct new() {
    _list = []
  }

  addConstraint(constraint) {
    _list.add(constraint)
  }

  size { _list.count }

  execute() {
    for (constraint in _list) {
      constraint.execute()
    }
  }
}

class Planner {
  construct new() {
    _currentMark = 0
  }

  // Attempt to satisfy the given constraint and, if successful,
  // incrementally update the dataflow graph.  Details: If satifying
  // the constraint is successful, it may override a weaker constraint
  // on its output. The algorithm attempts to resatisfy that
  // constraint using some other method. This process is repeated
  // until either a) it reaches a variable that was not previously
  // determined by any constraint or b) it reaches a constraint that
  // is too weak to be satisfied using any of its methods. The
  // variables of constraints that have been processed are marked with
  // a unique mark value so that we know where we've been. This allows
  // the algorithm to avoid getting into an infinite loop even if the
  // constraint graph has an inadvertent cycle.
  incrementalAdd(constraint) {
    var mark = newMark()
    var overridden = constraint.satisfy(mark)
    while (overridden != null) {
      overridden = overridden.satisfy(mark)
    }
  }

  // Entry point for retracting a constraint. Remove the given
  // constraint and incrementally update the dataflow graph.
  // Details: Retracting the given constraint may allow some currently
  // unsatisfiable downstream constraint to be satisfied. We therefore collect
  // a list of unsatisfied downstream constraints and attempt to
  // satisfy each one in turn. This list is traversed by constraint
  // strength, strongest first, as a heuristic for avoiding
  // unnecessarily adding and then overriding weak constraints.
  // Assume: [c] is satisfied.
  incrementalRemove(constraint) {
    var out = constraint.output
    constraint.markUnsatisfied()
    constraint.removeFromGraph()
    var unsatisfied = removePropagateFrom(out)
    var strength = REQUIRED
    while (true) {
      for (u in unsatisfied) {
        if (u.strength == strength) incrementalAdd(u)
      }
      strength = strength.nextWeaker
      if (strength == WEAKEST) break
    }
  }

  // Select a previously unused mark value.
  newMark() { _currentMark = _currentMark + 1 }

  // Extract a plan for resatisfaction starting from the given source
  // constraints, usually a set of input constraints. This method
  // assumes that stay optimization is desired; the plan will contain
  // only constraints whose output variables are not stay. Constraints
  // that do no computation, such as stay and edit constraints, are
  // not included in the plan.
  // Details: The outputs of a constraint are marked when it is added
  // to the plan under construction. A constraint may be appended to
  // the plan when all its input variables are known. A variable is
  // known if either a) the variable is marked (indicating that has
  // been computed by a constraint appearing earlier in the plan), b)
  // the variable is 'stay' (i.e. it is a constant at plan execution
  // time), or c) the variable is not determined by any
  // constraint. The last provision is for past states of history
  // variables, which are not stay but which are also not computed by
  // any constraint.
  // Assume: [sources] are all satisfied.
  makePlan(sources) {
    var mark = newMark()
    var plan = Plan.new()
    var todo = sources
    while (todo.count > 0) {
      var constraint = todo.removeAt(-1)
      if (constraint.output.mark != mark && constraint.inputsKnown(mark)) {
        plan.addConstraint(constraint)
        constraint.output.mark = mark
        addConstraintsConsumingTo(constraint.output, todo)
      }
    }
    return plan
  }

  // Extract a plan for resatisfying starting from the output of the
  // given [constraints], usually a set of input constraints.
  extractPlanFromConstraints(constraints) {
    var sources = []
    for (constraint in constraints) {
      // if not in plan already and eligible for inclusion.
      if (constraint.isInput && constraint.isSatisfied) sources.add(constraint)
    }
    return makePlan(sources)
  }

  // Recompute the walkabout strengths and stay flags of all variables
  // downstream of the given constraint and recompute the actual
  // values of all variables whose stay flag is true. If a cycle is
  // detected, remove the given constraint and answer
  // false. Otherwise, answer true.
  // Details: Cycles are detected when a marked variable is
  // encountered downstream of the given constraint. The sender is
  // assumed to have marked the inputs of the given constraint with
  // the given mark. Thus, encountering a marked node downstream of
  // the output constraint means that there is a path from the
  // constraint's output to one of its inputs.
  addPropagate(constraint, mark) {
    var todo = [constraint]
    while (todo.count > 0) {
      var d = todo.removeAt(-1)
      if (d.output.mark == mark) {
        incrementalRemove(constraint)
        return false
      }

      d.recalculate()
      addConstraintsConsumingTo(d.output, todo)
    }

    return true
  }

  // Update the walkabout strengths and stay flags of all variables
  // downstream of the given constraint. Answer a collection of
  // unsatisfied constraints sorted in order of decreasing strength.
  removePropagateFrom(out) {
    out.determinedBy = null
    out.walkStrength = WEAKEST
    out.stay = true
    var unsatisfied = []
    var todo = [out]
    while (todo.count > 0) {
      var v = todo.removeAt(-1)
      for (constraint in v.constraints) {
        if (!constraint.isSatisfied) unsatisfied.add(constraint)
      }

      var determining = v.determinedBy
      for (next in v.constraints) {
        if (next != determining && next.isSatisfied) {
          next.recalculate()
          todo.add(next.output)
        }
      }
    }

    return unsatisfied
  }

  addConstraintsConsumingTo(v, coll) {
    var determining = v.determinedBy
    for (constraint in v.constraints) {
      if (constraint != determining && constraint.isSatisfied) {
        coll.add(constraint)
      }
    }
  }
}

var total = 0

// This is the standard DeltaBlue benchmark. A long chain of equality
// constraints is constructed with a stay constraint on one end. An
// edit constraint is then added to the opposite end and the time is
// measured for adding and removing this constraint, and extracting
// and executing a constraint satisfaction plan. There are two cases.
// In case 1, the added constraint is stronger than the stay
// constraint and values must propagate down the entire length of the
// chain. In case 2, the added constraint is weaker than the stay
// constraint so it cannot be accomodated. The cost in this case is,
// of course, very low. Typical situations lie somewhere between these
// two extremes.
var chainTest = Fn.new {|n|
  ThePlanner = Planner.new()
  var prev = null
  var first = null
  var last = null

  // Build chain of n equality constraints.
  for (i in 0..n) {
    var v = Variable.new("v", 0)
    if (prev != null) EqualityConstraint.new(prev, v, REQUIRED)
    if (i == 0) first = v
    if (i == n) last = v
    prev = v
  }

  StayConstraint.new(last, STRONG_DEFAULT)
  var edit = EditConstraint.new(first, PREFERRED)
  var plan = ThePlanner.extractPlanFromConstraints([edit])
  for (i in 0...100) {
    first.value = i
    plan.execute()
    total = total + last.value
  }
}

var change = Fn.new {|v, newValue|
  var edit = EditConstraint.new(v, PREFERRED)
  var plan = ThePlanner.extractPlanFromConstraints([edit])
  for (i in 0...10) {
    v.value = newValue
    plan.execute()
  }

  edit.destroyConstraint()
}

// This test constructs a two sets of variables related to each
// other by a simple linear transformation (scale and offset). The
// time is measured to change a variable on either side of the
// mapping and to change the scale and offset factors.
var projectionTest = Fn.new {|n|
  ThePlanner = Planner.new()
  var scale = Variable.new("scale", 10)
  var offset = Variable.new("offset", 1000)
  var src = null
  var dst = null

  var dests = []
  for (i in 0...n) {
    src = Variable.new("src", i)
    dst = Variable.new("dst", i)
    dests.add(dst)
    StayConstraint.new(src, NORMAL)
    ScaleConstraint.new(src, scale, offset, dst, REQUIRED)
  }

  change.call(src, 17)
  total = total + dst.value
  if (dst.value != 1170) System.print("Projection 1 failed")

  change.call(dst, 1050)

  total = total + src.value
  if (src.value != 5) System.print("Projection 2 failed")

  change.call(scale, 5)
  for (i in 0...n - 1) {
    total = total + dests[i].value
    if (dests[i].value != i * 5 + 1000) System.print("Projection 3 failed")
  }

  change.call(offset, 2000)
  for (i in 0...n - 1) {
    total = total + dests[i].value
    if (dests[i].value != i * 5 + 2000) System.print("Projection 4 failed")
  }
}

var start = System.clock
for (i in 0...40) {
  chainTest.call(100)
  projectionTest.call(100)
}

System.print(total)
System.print("elapsed: %(System.clock - start)")
/*
 * Copyright (2020) The Delta Lake Project Authors.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.apache.spark.sql.catalyst.plans.logical

import java.util.Locale

import scala.collection.mutable

import org.apache.spark.sql.delta.sources.DeltaSQLConf

import org.apache.spark.sql.AnalysisException
import org.apache.spark.sql.catalyst.InternalRow
import org.apache.spark.sql.catalyst.analysis._
import org.apache.spark.sql.catalyst.expressions.{Alias, Attribute, AttributeReference, Expression, Literal, UnaryExpression}
import org.apache.spark.sql.catalyst.expressions.codegen.{CodegenContext, ExprCode}
import org.apache.spark.sql.internal.SQLConf
import org.apache.spark.sql.types.{ByteType, DataType, IntegerType, ShortType, StructField, StructType}
/**
 * A copy of Spark SQL Unevaluable for cross-version compatibility. In 3.0, implementers of
 * the original Unevaluable must explicitly override foldable to false; in 3.1 onwards, this
 * explicit override is invalid.
 */
trait DeltaUnevaluable extends Expression {
    
  final override def foldable: Boolean = false

  final override def eval(input: InternalRow = null): Any =
    throw new UnsupportedOperationException(s"Cannot evaluate expression: $this")

  final override protected def doGenCode(ctx: CodegenContext, ev: ExprCode): ExprCode =
    throw new UnsupportedOperationException(s"Cannot generate code for expression: $this")
   
}
/*

/**
 * Represents an action in MERGE's UPDATE or INSERT clause where a target columns is assigned the
 * value of an expression
 * @param targetColNameParts The name parts of the target column. This is a sequence to support
 *                           nested fields as targets.
 * @param expr Expression to generate the value of the target column.o
 */
case class DeltaMergeAction(targetColNameParts: Seq[String], expr: Expression)
  extends UnaryExpression with DeltaUnevaluable {
  override def child: Expression = expr
  override def dataType: DataType = expr.dataType
  override def sql: String = s"$targetColString = ${expr.sql}"
  override def toString: String = s"$targetColString = $expr"
  private lazy val targetColString: String = targetColNameParts.mkString("`", "`.`", "`")
}


/**
 * Trait that represents a WHEN clause in MERGE. See [[DeltaMergeInto]]. It extends [[Expression]]
 * so that Catalyst can find all the expressions in the clause implementations.
 */
sealed trait DeltaMergeIntoClause extends Expression with DeltaUnevaluable {
  /** Optional condition of the clause */
  def condition: Option[Expression]

  /**
   * Sequence of actions represented as expressions. Note that this can be only be either
   * UnresolvedStar, or MergeAction.
   */
  def actions: Seq[Expression]

  /**
   * Sequence of resolved actions represented as Aliases. Actions, once resolved, must
   * be Aliases and not any other NamedExpressions. So it should be safe to do this casting
   * as long as this is called after the clause has been resolved.
   */
  def resolvedActions: Seq[DeltaMergeAction] = {
    assert(actions.forall(_.resolved), "all actions have not been resolved yet")
    actions.map(_.asInstanceOf[DeltaMergeAction])
  }

  def clauseType: String =
    getClass.getSimpleName.stripPrefix("DeltaMergeInto").stripSuffix("Clause")

  override def toString: String = {
    val condStr = condition.map { c => s"condition: $c" }
    val actionStr = if (actions.isEmpty) None else {
      Some("actions: " + actions.mkString("[", ", ", "]"))
    }
    s"$clauseType " + Seq(condStr, actionStr).flatten.mkString("[", ", ", "]")
  }

  override def nullable: Boolean = false
  override def dataType: DataType = null
  override def children: Seq[Expression] = condition.toSeq ++ actions

  /** Verify whether the expressions in the actions are of the right type */
  protected[logical] def verifyActions(): Unit = actions.foreach {
    case _: UnresolvedStar =>
    case _: DeltaMergeAction =>
    case a => throw new IllegalArgumentException(s"Unexpected action expression $a in $this")
  }
}


object DeltaMergeIntoClause {
  /**
   * Convert the parsed columns names and expressions into action for MergeInto. Note:
   * - Size of column names and expressions must be the same.
   * - If the sizes are zeros and `emptySeqIsStar` is true, this function assumes
   *   that query had `*` as an action, and therefore generates a single action
   *   with `UnresolvedStar`. This will be expanded later during analysis.
   * - Otherwise, this will convert the names and expressions to MergeActions.
   */
  def toActions(
      colNames: Seq[UnresolvedAttribute],
      exprs: Seq[Expression],
      isEmptySeqEqualToStar: Boolean = true): Seq[Expression] = {
    assert(colNames.size == exprs.size)
    if (colNames.isEmpty && isEmptySeqEqualToStar) {
      Seq[Expression](UnresolvedStar(None))
    } else {
      colNames.zip(exprs).map { case (col, expr) => DeltaMergeAction(col.nameParts, expr) }
    }
  }

  def toActions(assignments: Seq[Assignment]): Seq[Expression] = {
    if (assignments.isEmpty) {
      Seq[Expression](UnresolvedStar(None))
    } else {
      assignments.map {
        case Assignment(key: UnresolvedAttribute, expr) => DeltaMergeAction(key.nameParts, expr)
        case Assignment(key: Attribute, expr) => DeltaMergeAction(Seq(key.name), expr)
        case other =>
          throw new AnalysisException(s"Unexpected assignment key: ${other.getClass} - $other")
      }
    }
  }
}

/** Trait that represents WHEN MATCHED clause in MERGE. See [[DeltaMergeInto]]. */
sealed trait DeltaMergeIntoMatchedClause extends DeltaMergeIntoClause

/** Represents the clause WHEN MATCHED THEN UPDATE in MERGE. See [[DeltaMergeInto]]. */
case class DeltaMergeIntoUpdateClause(condition: Option[Expression], actions: Seq[Expression])
  extends DeltaMergeIntoMatchedClause {

  def this(cond: Option[Expression], cols: Seq[UnresolvedAttribute], exprs: Seq[Expression]) =
    this(cond, DeltaMergeIntoClause.toActions(cols, exprs))
}

/** Represents the clause WHEN MATCHED THEN DELETE in MERGE. See [[DeltaMergeInto]]. */
case class DeltaMergeIntoDeleteClause(condition: Option[Expression])
    extends DeltaMergeIntoMatchedClause {
  def this(condition: Option[Expression], actions: Seq[DeltaMergeAction]) = this(condition)

  override def actions: Seq[Expression] = Seq.empty
}

/** Represents the clause WHEN NOT MATCHED THEN INSERT in MERGE. See [[DeltaMergeInto]]. */
case class DeltaMergeIntoInsertClause(condition: Option[Expression], actions: Seq[Expression])
  extends DeltaMergeIntoClause {

  def this(cond: Option[Expression], cols: Seq[UnresolvedAttribute], exprs: Seq[Expression]) =
    this(cond, DeltaMergeIntoClause.toActions(cols, exprs))
}

/**
 * Merges changes specified in the source plan into a target table, based on the given search
 * condition and the actions to perform when the condition is matched or not matched by the rows.
 *
 * The syntax of the MERGE statement is as follows.
 * {{{
 *    MERGE INTO <target_table_with_alias>
 *    USING <source_table_with_alias>
 *    ON <search_condition>
 *    [ WHEN MATCHED [ AND <condition> ] THEN <matched_action> ]
 *    [ WHEN MATCHED [ AND <condition> ] THEN <matched_action> ]
 *    ...
 *    [ WHEN NOT MATCHED [ AND <condition> ] THEN <not_matched_action> ]
 *    [ WHEN NOT MATCHED [ AND <condition> ] THEN <not_matched_action> ]
 *    ...
 *
 *    where
 *    <matched_action> = DELETE | UPDATE SET column1 = value1 [, column2 = value2 ...]
 *    <not_matched_action> = INSERT (column1 [, column2 ...]) VALUES (expr1 [, expr2 ...])
 * }}}
 *
 * - There can be any number of WHEN clauses.
 * - WHEN MATCHED clauses:
 *    - Each WHEN MATCHED clause can have an optional condition. However, if there are multiple
 * WHEN MATCHED clauses, only the last can omit the condition.
 *    - WHEN MATCHED clauses are dependent on their ordering; that is, the first clause that
 * satisfies the clause's condition has its corresponding action executed.
 * - WHEN NOT MATCHED clause:
 *    - Can only have the INSERT action. If present, they must follow the last WHEN MATCHED clause.
 *    - Each WHEN NOT MATCHED clause can have an optional condition. However, if there are multiple
 * clauses, only the last can omit the condition.
 *    - WHEN NOT MATCHED clauses are dependent on their ordering; that is, the first clause that
 * satisfies the clause's condition has its corresponding action executed.
 */
case class DeltaMergeInto(
    target: LogicalPlan,
    source: LogicalPlan,
    condition: Expression,
    matchedClauses: Seq[DeltaMergeIntoMatchedClause],
    notMatchedClauses: Seq[DeltaMergeIntoInsertClause],
    migratedSchema: Option[StructType] = None) extends Command {

  (matchedClauses ++ notMatchedClauses).foreach(_.verifyActions())

  override def children: Seq[LogicalPlan] = Seq(target, source)
  override def output: Seq[Attribute] = Seq.empty
}

object DeltaMergeInto {
  def apply(
      target: LogicalPlan,
      source: LogicalPlan,
      condition: Expression,
      whenClauses: Seq[DeltaMergeIntoClause]): DeltaMergeInto = {
    val deleteClauses = whenClauses.collect { case x: DeltaMergeIntoDeleteClause => x }
    val updateClauses = whenClauses.collect { case x: DeltaMergeIntoUpdateClause => x }
    val insertClauses = whenClauses.collect { case x: DeltaMergeIntoInsertClause => x }
    val matchedClauses = whenClauses.collect { case x: DeltaMergeIntoMatchedClause => x }

    // grammar enforcement goes here.
    if (whenClauses.isEmpty) {
      throw new AnalysisException("There must be at least one WHEN clause in a MERGE statement")
    }

    // check that only last MATCHED clause omits the condition
    if (matchedClauses.length > 1 && !matchedClauses.init.forall(_.condition.nonEmpty)) {
      throw new AnalysisException("When there are more than one MATCHED clauses in a MERGE " +
        "statement, only the last MATCHED clause can omit the condition.")
    }

    // check that only last NOT MATCHED clause omits the condition
    if (insertClauses.length > 1 && !insertClauses.init.forall(_.condition.nonEmpty)) {
      throw new AnalysisException("When there are more than one NOT MATCHED clauses in a MERGE " +
        "statement, only the last NOT MATCHED clause can omit the condition.")
    }

    DeltaMergeInto(
      target,
      source,
      condition,
      whenClauses.collect { case x: DeltaMergeIntoMatchedClause => x },
      whenClauses.collect { case x: DeltaMergeIntoInsertClause => x },
      None)
  }

  def resolveReferences(merge: DeltaMergeInto, conf: SQLConf)(
      resolveExpr: (Expression, LogicalPlan) => Expression): DeltaMergeInto = {

    val DeltaMergeInto(
        target, source, condition, matchedClauses, notMatchedClause, migratedSchema) =
      merge

    // We must do manual resolution as the expressions in different clauses of the MERGE have
    // visibility of the source, the target or both. Additionally, the resolution logic operates
    // on the output of the `children` of the operator in question. Since for MERGE we must
    // consider each child separately, we also must make these dummy nodes to avoid
    // "skipping" the effects of the top node in these query plans.

    val fakeSourcePlan = Project(source.output, source)
    val fakeTargetPlan = Project(target.output, target)

    /**
     * Resolves expression with given plan or fail using given message. It makes a best-effort
     * attempt to throw specific error messages on which part of the query has a problem.
     */
    def resolveOrFail(expr: Expression, plan: LogicalPlan, mergeClauseType: String): Expression = {
      val resolvedExpr = resolveExpr(expr, plan)
      resolvedExpr.flatMap(_.references).filter(!_.resolved).foreach { a =>
        // Note: This will throw error only on unresolved attribute issues,
        // not other resolution errors like mismatched data types.
        val cols = "columns " + plan.children.flatMap(_.output).map(_.sql).mkString(", ")
        a.failAnalysis(s"cannot resolve ${a.sql} in $mergeClauseType given $cols")
      }
      resolvedExpr
    }


    val shouldAutoMigrate = conf.getConf(DeltaSQLConf.DELTA_SCHEMA_AUTO_MIGRATE)
    // Migrated schema to be used for schema evolution.
    val finalSchema = if (shouldAutoMigrate) {
      // We can't just use the merge method in StructType, because it doesn't account
      // for possible implicit conversions. Instead, we use the target schema for all
      // existing columns and the source schema only for new ones.
      val targetSchema = target.schema
      val migratedSchema = mutable.ListBuffer[StructField]()
      targetSchema.foreach(migratedSchema.append(_))

      source.schema.foreach { col =>
        val isInTarget = targetSchema.exists { targetCol =>
          target.conf.resolver(targetCol.name, col.name)
        }
        if (!isInTarget) { migratedSchema.append(col) }
      }

      StructType(migratedSchema)
    } else {
      target.schema
    }

    /**
     * Resolves a clause using the given plan (used for resolving the action exprs) and
     * returns the resolved clause.
     */
    def resolveClause[T <: DeltaMergeIntoClause](clause: T, planToResolveAction: LogicalPlan): T = {
      val typ = clause.clauseType.toUpperCase(Locale.ROOT)

      val resolvedActions: Seq[DeltaMergeAction] = clause.actions.flatMap { action =>
        action match {
          // For actions like `UPDATE SET *` or `INSERT *`
          case _: UnresolvedStar if !shouldAutoMigrate =>
            // Expand `*` into seq of [ `columnName = sourceColumnBySameName` ] for every target
            // column name. The target columns do not need resolution. The right hand side
            // expression (i.e. sourceColumnBySameName) needs to be resolved only by the source
            // plan.
            fakeTargetPlan.output.map(_.name).map { tgtColName =>
              val resolvedExpr = resolveOrFail(
                UnresolvedAttribute.quotedString(s"`$tgtColName`"),
                fakeSourcePlan, s"$typ clause")
              DeltaMergeAction(Seq(tgtColName), resolvedExpr)
            }
          case _: UnresolvedStar if shouldAutoMigrate =>
            clause match {
              case _: DeltaMergeIntoInsertClause =>
                // Expand `*` into seq of [ `columnName = sourceColumnBySameName` ] for every source
                // and target column name. Columns that exist only in the target are set to null,
                // and columns that exist only in the source will be added later through
                // schema evolution.
                finalSchema.map { col =>
                  // Construct an action that sets the target column to either the source column if
                  // it's present or NULL otherwise.
                  val sourceExpr = source.output.find { a =>
                    conf.resolver(a.name, col.name)
                  }.getOrElse {
                    Alias(Literal.create(null, col.dataType), col.name)()
                  }

                  DeltaMergeAction(Seq(col.name), sourceExpr)
                }
              case clause: DeltaMergeIntoUpdateClause =>
                // Expand `*` into seq of [ `columnName = sourceColumnBySameName` ] for every source
                // and target column name. Columns that exist only in the target are set to
                // themselves to produce a no-op, and columns that exist only in the source will be
                // added later through schema evolution.
                finalSchema.map { col =>
                  // Construct an action that sets the target column to the source column if present
                  // or the current column value otherwise. (The no-op action is required because
                  // later code will assume actions are present for all target columns.)
                  val sourceExpr = source.output.find { a =>
                    conf.resolver(a.name, col.name)
                  }.orElse {
                    target.output.find { a =>
                      conf.resolver(a.name, col.name)
                    }
                  }.getOrElse {
                    // This shouldn't be able to happen - every column in the merged result has
                    // gotta resolve in either the target or the source.
                    clause.failAnalysis(
                      s"cannot expand $col from merged schema ${finalSchema.prettyJson} in " +
                        s"UPDATE *, given source ${source.output}, " +
                        s"target ${target.output}")
                  }

                  DeltaMergeAction(Seq(col.name), sourceExpr)
                }
            }

          // For actions like `UPDATE SET x = a, y = b` or `INSERT (x, y) VALUES (a, b)`
          case DeltaMergeAction(colNameParts, expr) =>

            val unresolvedAttrib = UnresolvedAttribute(colNameParts)
            val resolutionErrorMsg =
              s"Cannot resolve ${unresolvedAttrib.sql} in target columns in $typ " +
                s"clause given columns ${target.output.map(_.sql).mkString(", ")}"

            // Resolve the target column name without database/table/view qualifiers
            // If clause allows nested field to be target, then this will return the all the
            // parts of the name (e.g., "a.b" -> Seq("a", "b")). Otherwise, this will
            // return only one string.
            val resolvedNameParts = DeltaUpdateTable.getTargetColNameParts(
              resolveOrFail(unresolvedAttrib, fakeTargetPlan, s"$typ clause"), resolutionErrorMsg)

            val resolvedExpr = resolveOrFail(expr, planToResolveAction, s"$typ clause")
            Seq(DeltaMergeAction(resolvedNameParts, resolvedExpr))

          case _ =>
            action.failAnalysis(s"Unexpected action expression '$action' in clause $clause")
        }
      }

      val resolvedCondition =
        clause.condition.map(resolveOrFail(_, planToResolveAction, s"$typ condition"))
      clause.makeCopy(Array(resolvedCondition, resolvedActions)).asInstanceOf[T]
    }

    // Resolve everything
    val resolvedCond = resolveOrFail(condition, merge, "search condition")
    val resolvedMatchedClauses = matchedClauses.map {
      resolveClause(_, merge)
    }
    val resolvedNotMatchedClause = notMatchedClause.map {
      resolveClause(_, fakeSourcePlan)
    }
    val resolvedMerge = DeltaMergeInto(
      target, source, resolvedCond,
      resolvedMatchedClauses, resolvedNotMatchedClause,
      if (shouldAutoMigrate) Some(finalSchema) else None)

    // Its possible that pre-resolved expressions (e.g. `sourceDF("key") = targetDF("key")`) have
    // attribute references that are not present in the output attributes of the children (i.e.,
    // incorrect DataFrame was used in the `df("col")` form).
    if (resolvedMerge.missingInput.nonEmpty) {
      val missingAttributes = resolvedMerge.missingInput.mkString(",")
      val input = resolvedMerge.inputSet.mkString(",")
      val msgForMissingAttributes = s"Resolved attribute(s) $missingAttributes missing " +
        s"from $input in operator ${resolvedMerge.simpleString(SQLConf.get.maxToStringFields)}."
      resolvedMerge.failAnalysis(msgForMissingAttributes)
    }
    resolvedMerge
  }
}*/
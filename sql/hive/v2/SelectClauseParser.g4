/**
   Licensed to the Apache Software Foundation (ASF) under one or more
   contributor license agreements.  See the NOTICE file distributed with
   this work for additional information regarding copyright ownership.
   The ASF licenses this file to You under the Apache License, Version 2.0
   (the "License"); you may not use this file except in compliance with
   the License.  You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.

   @author Canwei He
*/

// $antlr-format alignTrailingComments true, columnLimit 150, minEmptyLines 1, maxEmptyLinesToKeep 1, reflowComments false, useTab false
// $antlr-format allowShortRulesOnASingleLine false, allowShortBlocksOnASingleLine true, alignSemicolons hanging, alignColons hanging

parser grammar SelectClauseParser;

@members {
    public enum Standard { V1, V2, V3 }
    private Standard standard = Standard.V2;
    public void setStandard(Standard s) { standard = s; }
    private boolean atLeast(Standard s) { return standard.ordinal() >= s.ordinal(); }
    public boolean isV1OrLater() { return atLeast(Standard.V1); }
    public boolean isV2OrLater() { return atLeast(Standard.V2); }
    public boolean isV3OrLater() { return atLeast(Standard.V3); }
}

//----------------------- Rules for parsing selectClause -----------------------------
// select a,b,c ...
selectClause
    : KW_SELECT QUERY_HINT? (
        ((KW_ALL | KW_DISTINCT)? selectList)
        | (KW_TRANSFORM selectTrfmClause)
    )
    | trfmClause
    ;

selectList
    : selectItem (COMMA selectItem)*
    ;

selectTrfmClause
    : LPAREN selectExpressionList RPAREN rowFormat? recordWriter? KW_USING StringLiteral (
        KW_AS ((LPAREN (aliasList | columnNameTypeList) RPAREN) | (aliasList | columnNameTypeList))
    )? rowFormat? recordReader?
    ;

selectItem
    : tableAllColumns
    | (expression ((KW_AS? identifier) | (KW_AS LPAREN identifier (COMMA identifier)* RPAREN))?)
    ;

trfmClause
    : (KW_MAP selectExpressionList | KW_REDUCE selectExpressionList) rowFormat? recordWriter? KW_USING StringLiteral (
        KW_AS ((LPAREN (aliasList | columnNameTypeList) RPAREN) | (aliasList | columnNameTypeList))
    )? rowFormat? recordReader?
    ;

selectExpression
    : tableAllColumns
    | expression
    ;

selectExpressionList
    : selectExpression (COMMA selectExpression)*
    ;

//---------------------- Rules for windowing clauses -------------------------------
window_clause
    : KW_WINDOW window_defn (COMMA window_defn)*
    ;

window_defn
    : identifier KW_AS window_specification
    ;

window_specification
    : (identifier | ( LPAREN identifier? partitioningSpec? window_frame? RPAREN))
    ;

window_frame
    : window_range_expression
    | window_value_expression
    ;

window_range_expression
    : KW_ROWS window_frame_start_boundary
    | KW_ROWS KW_BETWEEN window_frame_boundary KW_AND window_frame_boundary
    ;

window_value_expression
    : KW_RANGE window_frame_start_boundary
    | KW_RANGE KW_BETWEEN window_frame_boundary KW_AND window_frame_boundary
    ;

window_frame_start_boundary
    : KW_UNBOUNDED KW_PRECEDING
    | KW_CURRENT KW_ROW
    | Number KW_PRECEDING
    ;

window_frame_boundary
    : KW_UNBOUNDED (KW_PRECEDING | KW_FOLLOWING)
    | KW_CURRENT KW_ROW
    | Number (KW_PRECEDING | KW_FOLLOWING)
    ;

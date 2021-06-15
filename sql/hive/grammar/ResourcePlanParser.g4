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
parser grammar ResourcePlanParser;


resourcePlanDdlStatements
    : createResourcePlanStatement
    | alterResourcePlanStatement
    | dropResourcePlanStatement
    | globalWmStatement
    | replaceResourcePlanStatement
    | createTriggerStatement
    | alterTriggerStatement
    | dropTriggerStatement
    | createPoolStatement
    | alterPoolStatement
    | dropPoolStatement
    | createMappingStatement
    | alterMappingStatement
    | dropMappingStatement
    ;

rpAssign
  : (KW_QUERY_PARALLELISM EQUAL Number)
  | (KW_DEFAULT KW_POOL EQUAL poolPath)
  ;

rpAssignList
  : rpAssign (COMMA rpAssign)*
  ;

rpUnassign
  : (KW_QUERY_PARALLELISM)
  | (KW_DEFAULT KW_POOL)
  ;

rpUnassignList
  : rpUnassign (COMMA rpUnassign)*
  ;

createResourcePlanStatement
    : KW_CREATE KW_RESOURCE KW_PLAN (
          (identifier KW_LIKE identifier)
        | (identifier (KW_WITH rpAssignList)?)
      )
    ;


withReplace
    : KW_WITH KW_REPLACE
    ;


activate
    : KW_ACTIVATE withReplace?
    ;


enable
    : KW_ENABLE
    ;


disable
    : KW_DISABLE
    ;


unmanaged
    : KW_UNMANAGED
    ;


alterResourcePlanStatement
    : KW_ALTER KW_RESOURCE KW_PLAN identifier (
          (KW_VALIDATE)
        | (KW_DISABLE)
        | (KW_SET rpAssignList)
        | (KW_UNSET rpUnassignList)
        | (KW_RENAME KW_TO identifier)
        | ((activate enable? | enable activate?))
      )
    ;

/** It might make sense to make this more generic, if something else could be enabled/disabled.
    For now, it's only used for WM. Translate into another form of an alter statement. */
globalWmStatement
    : (enable | disable) KW_WORKLOAD KW_MANAGEMENT
    ;

replaceResourcePlanStatement
    : KW_REPLACE (
          (KW_ACTIVE KW_RESOURCE KW_PLAN KW_WITH identifier)
        | (KW_RESOURCE KW_PLAN identifier KW_WITH identifier)
      )
    ;

dropResourcePlanStatement
    : KW_DROP KW_RESOURCE KW_PLAN identifier
    ;

poolPath
    : identifier (DOT identifier)*
    ;

triggerExpression
    : triggerAtomExpression
    ;

triggerExpressionStandalone
    : triggerExpression EOF ;

/*
  The rules triggerOrExpression and triggerAndExpression are not being used right now.
  Only > operator is supported, this should be changed if logic in ExpressionFactory changes.
*/
triggerOrExpression
    : triggerAndExpression (KW_OR triggerAndExpression)*
    ;

triggerAndExpression
    : triggerAtomExpression (KW_AND triggerAtomExpression)*
    ;

triggerAtomExpression
    : identifier comparisionOperator triggerLiteral
    ;


triggerLiteral
    : Number
    | StringLiteral
    ;

comparisionOperator
    : GREATERTHAN
    ;

triggerActionExpression
    : KW_KILL
    | (KW_MOVE KW_TO poolPath)
    ;

triggerActionExpressionStandalone
    : triggerActionExpression EOF ;

createTriggerStatement
    : KW_CREATE KW_TRIGGER identifier DOT identifier
      KW_WHEN triggerExpression KW_DO triggerActionExpression
    ;

alterTriggerStatement
    : KW_ALTER KW_TRIGGER identifier DOT identifier (
        (KW_WHEN triggerExpression KW_DO triggerActionExpression)
      | (KW_ADD KW_TO KW_POOL poolPath)
      | (KW_DROP KW_FROM KW_POOL poolPath)
      | (KW_ADD KW_TO KW_UNMANAGED)
      | (KW_DROP KW_FROM KW_UNMANAGED)
    )
    ;


dropTriggerStatement
    : KW_DROP KW_TRIGGER identifier DOT identifier
    ;

poolAssign
    : (
        (KW_ALLOC_FRACTION EQUAL Number)
      | (KW_QUERY_PARALLELISM EQUAL Number)
      | (KW_SCHEDULING_POLICY EQUAL StringLiteral)
      | (KW_PATH EQUAL poolPath)
      )
    ;

poolAssignList
    : poolAssign (COMMA poolAssign)*
    ;

createPoolStatement
    : KW_CREATE KW_POOL identifier DOT poolPath
      KW_WITH poolAssignList
    ;

alterPoolStatement
    : KW_ALTER KW_POOL identifier DOT poolPath (
        (KW_SET poolAssignList)
        | (KW_UNSET KW_SCHEDULING_POLICY)
        | (KW_ADD KW_TRIGGER identifier)
        | (KW_DROP KW_TRIGGER identifier)
      )
    ;

dropPoolStatement
    : KW_DROP KW_POOL identifier DOT poolPath
    ;

createMappingStatement
    : (KW_CREATE (KW_USER | KW_GROUP | KW_APPLICATION)
         KW_MAPPING StringLiteral
         KW_IN identifier ((KW_TO poolPath) | unmanaged)
         (KW_WITH KW_ORDER Number)?)
    ;

alterMappingStatement
    : (KW_ALTER (KW_USER | KW_GROUP | KW_APPLICATION)
         KW_MAPPING StringLiteral
         KW_IN identifier ((KW_TO poolPath) | unmanaged)
         (KW_WITH KW_ORDER Number)?)
    ;

dropMappingStatement
    : KW_DROP (KW_USER | KW_GROUP | KW_APPLICATION) KW_MAPPING
         StringLiteral KW_IN identifier
    ;
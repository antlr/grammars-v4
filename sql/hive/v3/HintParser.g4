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

parser grammar HintParser;

options
{
    tokenVocab = HiveLexer;
}

@members {
    public enum Standard { V1, V2, V3 }
    private Standard standard = Standard.V2;
    public void setStandard(Standard s) { standard = s; }
    private boolean atLeast(Standard s) { return standard.ordinal() >= s.ordinal(); }
    public boolean isV1OrLater() { return atLeast(Standard.V1); }
    public boolean isV2OrLater() { return atLeast(Standard.V2); }
    public boolean isV3OrLater() { return atLeast(Standard.V3); }
}

// starting rule
hint
    : hintList EOF
    ;

hintList
    : hintItem (COMMA hintItem)*
    ;

hintItem
    : hintName (LPAREN hintArgs RPAREN)?
    ;

hintName
    : KW_MAPJOIN
    | KW_SEMI
    | KW_STREAMTABLE
    ;

hintArgs
    : hintArgName (COMMA hintArgName)*
    ;

hintArgName
    : Identifier
    | Number
    | KW_NONE
    ;

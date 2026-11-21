VERSION 1.0 CLASS
BEGIN
    MultiUse = -1  'True
END
Attribute VB_Name = "SQLSelect"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = True
Option Explicit

' Class: SQLSelect
' A SQL Select query
Implements iSQLQuery

Private oSQL As SQLQuery
Private vFields() As Variant
Private bDistinct As Boolean
Private vGroupBy As Variant
Private oHaving As SQLCondition
Private oHavingGroup As SQLWhereGroup
Private aJoin() As Variant              'An array of arrays. Each array is a 'from_item'
Private aOrder() As Variant
Private sUnion() As Variant
'Limit
'Offset

' Property: Table
Property Let Table(sValue As String)
    addTable sValue, ""
End Property

' Property: Fields
' The fields in the query
Property Let Fields(vValue)
    Dim element As Variant
    For Each element In vValue
        AddField element
    Next
End Property

' Property: GroupBy
' The field to use for aggregration
Property Let GroupBy(vValue)
    vGroupBy = vValue
End Property

' Constructor: Class_Initialize
' Initializes class members
Private Sub Class_Initialize()
    ReDim aJoin(0)
    ReDim vFields(0)
    ReDim aOrder(0)
    vFields(0) = ""
    aOrder(0) = ""
    bDistinct = False
    vGroupBy = Array()
    Set oSQL = New SQLQuery
End Sub

' Sub: addField
' Add a field to the query statement
Public Sub AddField(sField, Optional sAlias As String = "")
    Dim ArrLen As Integer
    ArrLen = UBound(vFields)
    If ArrLen = 0 Then
        If Not IsArray(vFields(0)) Then
            ArrLen = -1
        End If
    End If
    ReDim Preserve vFields(0 To ArrLen + 1)
    vFields(ArrLen + 1) = Array(sField, sAlias)
End Sub

' Sub: addTable
' Add a table to the query statement
Public Sub addTable(sName As String, Optional sAlias As String = "")
    aJoin = ArrayPush(aJoin, Array("", sName, sAlias, ""))
End Sub

' Sub: AddHaving
' Add a having clause o the SQL statement
Public Sub AddHaving(Field, Value, Optional op As String = "=", Optional GroupType As String = "AND")
    Dim NewHaving As New SQLCondition
    If Not (oHaving Is Nothing) Then
        NewHaving.Create Field, op, Value
        Set oHavingGroup = New SQLWhereGroup
        oHavingGroup.SetGroup oHaving, NewHaving, GroupType
        
        'Clear SQLWhere since we are using SQLWhereGroup instead
        Set oHaving = Nothing
    ElseIf oHavingGroup Is Nothing Then
        Set oHaving = New SQLCondition
        oHaving.Create Field, op, Value
    Else
        NewHaving.Create Field, op, Value
        oHavingGroup.AddWhere NewHaving, GroupType
    End If

End Sub

' Sub: AddArgument
' Add an argument to the SQL Statement.
' An argument is a value for a placeholder.
Public Sub AddArgument(sName As String, vValue)
    oSQL.AddArgument sName, vValue
End Sub

' Sub: AddJoin
' Add a join condition to the SQL query
Public Sub AddJoin(sType As String, sTable As String, Optional sAlias As String = "", Optional sCondition As String = "")
    'Should Check that sType is either "INNER", "LEFT OUTER", or "RIGHT OUTER"
    Dim JoinLen As Integer
    JoinLen = UBound(aJoin)
    ReDim Preserve aJoin(0 To JoinLen + 1)
    aJoin(JoinLen + 1) = Array(sType, sTable, sAlias, sCondition)
End Sub

' Sub: InnerJoin
' Add an inner join to the SQL query
Public Sub InnerJoin(sTable As String, sAlias As String, sCondition As String)
    AddJoin "INNER", sTable, sAlias, sCondition
End Sub

' Sub: LeftJoin 
' Add a left join to the SQL query
Public Sub LeftJoin(sTable, sAlias, Optional sCondition As String = "")
    AddJoin "LEFT OUTER", sTable, sAlias, sCondition
End Sub

' Sub: RightJoin
' Add a right join to the SQL query
' Left Joins are prefered over Right. Please edit query to use a left join
Public Sub RightJoin(sTable, sAlias, Optional sCondition As String = "")
    AddJoin "RIGHT OUTER", sTable, sAlias, sCondition
End Sub

' Sub: Union
' Join two SQLSelect objects together with a union
' sType is either "", "ALL", or "DISTINCT"
Public Sub Union(oSelect As SQLSelect, Optional sType = "")
    Dim UnionArray() As Variant
    UnionArray = Array(SQLSelect, sType)
    'Add UnionArray to aUnion
End Sub

' Sub: Distinct
' Set the distict flag to true or false
Public Sub Distinct(Optional bValue = True)
    bDistinct = bValue
End Sub

' Sub: AddWhere
' Add a where clause to the SQL query
Public Sub AddWhere(Field, Value, Optional op As String = "=", Optional GroupType As String = "AND")
    oSQL.AddWhere Field, Value, op, GroupType
End Sub

Public Sub AddWhereGroup(NewWhereGroup As SQLWhereGroup, Optional GroupType As String = "AND")
    oSQL.AddWhereGroup NewWhereGroup, GroupType
End Sub

Public Sub OrderBy(sField As String, Optional sDirection As String = "ASC")
    If sDirection = "DESC" Then
        sDirection = "DESC"
    Else
        sDirection = "ASC"
    End If
    Dim ArrLen As Integer
    ArrLen = UBound(aOrder)
    If ArrLen = 0 Then
        If aOrder(0) = "" Then
            ArrLen = -1
        End If
    End If
    ReDim Preserve aOrder(0 To ArrLen + 1)
    aOrder(ArrLen + 1) = Array(sField, sDirection)
End Sub

' Function: getByProperty
' Generate a query of the form
' SELECT sField FROM sTableValue WHERE sProperty = vValue
'
' Parameters:
'    sTableValue - The table to select the data from
'    sField - The field name
'    sProperty - The field to filter by
'    vValue - The value to filter by
Public Sub getByProperty(sTableValue As String, sField As String, sProperty As String, vValue)
    addTable sTableValue, ""
    AddField sField
    AddWhere sProperty, vValue
End Sub

' Function: iSQLQuery_ToString
' Create the query string
Public Function iSQLQuery_ToString() As String
    Dim return_string As String

    If UBound(vFields) < 0 Then
        return_string = ""
    Else
        return_string = "SELECT " & DistinctString & FieldList & " FROM " & _
            JoinString & oSQL.WhereString & GroupByString & HavingString & OrderByString
    End If
    iSQLQuery_ToString = oSQL.ReplaceArguments(return_string)
End Function

Private Function DistinctString() As String
    Dim sDistinct As String
    sDistinct = ""
    If bDistinct Then
        sDistinct = "DISTINCT "
    End If
    DistinctString = sDistinct
End Function

Private Function JoinString()
    Dim R As Long
    Dim Lines() As String
    Dim Line As String
    Dim LineArray As Variant
    ReDim Lines(0 To UBound(aJoin))
    For R = 0 To UBound(aJoin)
        LineArray = aJoin(R)
        Line = ""
        If LineArray(0) <> "" Then
            Line = LineArray(0) & " JOIN "
        ElseIf R > 0 Then
            Lines(R - 1) = Lines(R - 1) & ","
        End If
        Line = Line & LineArray(1)
        If LineArray(2) <> "" Then
            Line = Line & " " & LineArray(2)
        End If
        If LineArray(3) <> "" Then
            Line = Line & " ON " & LineArray(3)
        End If
        Lines(R) = Line
    Next R
    JoinString = Join(Lines, " ")
End Function

Private Function GroupByString() As String
    If UBound(vGroupBy) > -1 Then
        GroupByString = " GROUP BY " & Join(vGroupBy, ", ")
    End If
End Function

Private Function HavingString() As String
    Dim sHaving As String
    If Not (oHaving Is Nothing And oHavingGroup Is Nothing) Then
        If Not (oHaving Is Nothing) Then
            sHaving = oHaving.toString
        Else
            sHaving = oHavingGroup.toString
        End If
        HavingString = " HAVING " & sHaving
    End If
End Function

Private Function OrderByString() As String
    OrderByString = ""
    If IsArray(aOrder(0)) Then
        OrderByString = " ORDER BY " & JoinArrayofArrays(aOrder)
    End If
End Function

Private Function FieldList()
    Dim R As Long
    Dim Lines() As String
    Dim Line As String
    Dim LineArray As Variant
    ReDim Lines(0 To UBound(vFields))
    For R = 0 To UBound(vFields)
        LineArray = vFields(R)
        Line = LineArray(0)
        If LineArray(1) <> "" Then
            Line = Line & " AS " & LineArray(1)
        End If
        Lines(R) = Line
    Next R
    FieldList = Join(Lines, ", ")
End Function

Private Function UnionString()
    Dim NewSelect As iSQLQuery
    aUnion = UnionArray(0)
    Set NewSelect = aUnion(0)
    UnionString = " UNION " & NewSelect.toString()
End Function

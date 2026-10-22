Attribute VB_Name = "SQLHelperFunctions"
Public Function toUnix(dt) As Long
    toUnix = DateDiff("s", "1/1/1970", dt)
End Function

Public Function toISO(dt) As String
    toISO = Format(dt, "YYYY-MM-DD") & "T" & Format(dt, "HH:MM:SS")
End Function

Public Function str(vValue) As String
    str = "'" & Replace(vValue, "'", "''") & "'"
End Function

Function JoinArrayofArrays(ByVal vArray As Variant, _
                Optional ByVal WordDelim As String = " ", _
                Optional ByVal LineDelim As String = vbNewLine) As String
    Dim R As Long, Lines() As String
    ReDim Lines(0 To UBound(vArray))
    For R = 0 To UBound(vArray)
        Dim InnerArray() As Variant
        InnerArray = vArray(R)
        Lines(R) = Join(InnerArray, WordDelim)
    Next
    JoinArrayofArrays = Join(Lines, LineDelim)
End Function

Function getDimension(Var As Variant) As Long
    On Error GoTo Err
    Dim i As Long
    Dim tmp As Long
    i = 0
    Do While True
        i = i + 1
        tmp = UBound(Var, i)
    Loop

    Err:
        getDimension = i - 1
End Function

Public Sub QuickSort(vArray As Variant, inLow As Long, inHi As Long)
    Dim pivot   As Variant
    Dim tmpSwap As Variant
    Dim tmpLow  As Long
    Dim tmpHi   As Long

    tmpLow = inLow
    tmpHi = inHi

    pivot = vArray((inLow + inHi) \ 2)

    While (tmpLow <= tmpHi)

        While (vArray(tmpLow) < pivot And tmpLow < inHi)
            tmpLow = tmpLow + 1
        Wend

        While (pivot < vArray(tmpHi) And tmpHi > inLow)
            tmpHi = tmpHi - 1
        Wend

        If (tmpLow <= tmpHi) Then
            tmpSwap = vArray(tmpLow)
            vArray(tmpLow) = vArray(tmpHi)
            vArray(tmpHi) = tmpSwap
            tmpLow = tmpLow + 1
            tmpHi = tmpHi - 1
        End If
    Wend
    If (inLow < tmpHi) Then
        QuickSort vArray, inLow, tmpHi
    End If
    If (tmpLow < inHi) Then
        QuickSort vArray, tmpLow, inHi
    End If
End Sub

Public Function ArrayPush(vArray As Variant, newValue As Variant)
    ArrLen = UBound(vArray)
    If IsEmpty(vArray(0)) Then
        ArrLen = -1
    End If
    ReDim Preserve vArray(0 To ArrLen + 1)
    vArray(ArrLen + 1) = newValue
    ArrayPush = vArray
End Function

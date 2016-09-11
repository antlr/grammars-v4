Public Sub Module()
    X = 0
    Y = 1
    Z = 2
    Test1 = X + Y * Z
    Test2 = X - Y * Z
    Test3 = Z + X * Y - Y * Z
    Test4 = Z + X * Y Mod Z * 2 + 5 * Z
    Test5 = Z + X ^ 3 * Y Mod Z * 2 + 5 * Z ^ X
End Sub
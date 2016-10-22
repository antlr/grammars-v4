Private Function Function1()
    Function1 = 1
End Function

Private Function Function2(I, J)
    Function2 = 2
End Function

Private Function Sub1()
    Sub1 = 3
End Function

Private Function Sub2(I, J)
    Sub2 = 4
End Function

Private Sub TestSimpleCalls()
    Dim SomeVariable(1)

    ''
    '' explicit call statement in block
    ''
    
    '' Call SomeVariable

    Call Function1
    '' Call Function1 ()
    
    '' Call Function2 1, 2
    Call Function2(1, 2)
    
    Call Sub1
    '' Call Sub1 ()
    
    '' Call Sub2 1, 2
    Call Sub2(1, 2)



    ''
    '' implicit call statement in block
    ''

    '' SomeVariable

    Function1
    '' Function1()
    
    Function2 1, 2
    '' Function2 (1, 2)
    
    Sub1
    '' Sub1()
    
    Sub2 1, 2
    '' Sub2 (1, 2)


    ''
    '' implicit call statement in statement
    ''
    Dim I
    
    I = SomeVariable
    '' I = SomeVariable 0
    I = SomeVariable(0) '' array element call
    
    I = Function1
    I = Function1()
    '' I = Call Function1
    '' I = Call Function1()
    
    '' I = Function2 1, 2
    I = Function2(1, 2)
    
    I = Sub1
    I = Sub1()
    '' I = Call Sub1()
    
    '' I = Sub2 1, 2
    I = Sub2(1, 2)
    '' I = Call Sub2(1, 2)
End Sub

Private Sub TestMemberCalls(Module As Module1)
    ''
    '' explicit call statement in block
    ''
    
    '' Call Module.SomeVariable
    '' Call Module.SomeVariable(1)

    Call Module.Function1
    '' Call Module.Function1 ()
    
    '' Call Module.Function2 1, 2
    Call Module.Function2(1, 2)
    
    Call Module.Sub1
    '' Call Module.Sub1 ()
    
    '' Call Module.Sub2 1, 2
    Call Module.Sub2(1, 2)



    ''
    '' implicit call statement in block
    ''

    '' Module.SomeVariable

    Module.Function1
    '' Module.Function1()
    
    Module.Function2 1, 2
    '' Module.Function2 (1, 2)
    
    Module.Sub1
    '' Module.Sub1()
    
    Module.Sub2 1, 2
    '' Module.Sub2 (1, 2)


    ''
    '' implicit call statement in statement
    ''
    Dim I
    
    I = Module.SomeVariable
    
    I = Module.Function1
    I = Module.Function1()
    '' I = Call Module.Function1
    '' I = Call Module.Function1()
    
    '' I = Module.Function2 1, 2
    I = Module.Function2(1, 2)
    
    '' I = Module.Sub1 '' function or variable expected
    '' I = Module.Sub1() '' function or variable expected
    '' I = Call Module.Sub1() '' function or variable expected
    
    '' I = Module.Sub2 1, 2 '' function or variable expected
    '' I = Module.Sub2(1, 2) '' function or variable expected
    '' I = Call Module.Sub2(1, 2) '' function or variable expected
End Sub

Private Sub TestMemberCallsRecursive(Module As Module1)
    ''
    '' explicit call statement in block
    ''
    
    '' Call Module.SomeModule.SomeVariable
    '' Call Module.SomeModule().SomeVariable
    '' Call Module.GetModule.SomeVariable
    '' Call Module.GetModule().SomeVariable


    Call Module.SomeModule.Function1
    Call Module.SomeModule().Function1
    Call Module.GetModule.Function1
    Call Module.GetModule().Function1
    Call Module.GetModule().GetModule().Function1
    '' Call Module.GetModule.Function1 ()
    '' Call Module.SomeModule.Function1 ()
    
    '' Call Module.GetModule.Function2 1, 2
    '' Call Module.SomeModule.Function2 1, 2
    Call Module.GetModule.Function2(1, 2)
    Call Module.SomeModule.Function2(1, 2)
    Call Module.GetModule().Function2(1, 2)
    Call Module.SomeModule().Function2(1, 2)
    
    Call Module.GetModule.Sub1
    Call Module.GetModule().Sub1
    '' Call Module.GetModule.Sub1 ()
    
    '' Call Module.GetModule.Sub2 1, 2
    Call Module.GetModule.Sub2(1, 2)
    Call Module.GetModule().Sub2(1, 2)



    ''
    '' implicit call statement in block
    ''

    '' Module.SomeModule.SomeVariable

    Module.GetModule().Function1
    '' Module.GetModule().Function1()
    
    Module.GetModule().Function2 1, 2
    '' Module.GetModule().Function2 (1, 2)
    
    Module.GetModule().Sub1
    '' Module.GetModule().Sub1()
    
    Module.GetModule().Sub2 1, 2
    '' Module.GetModule().Sub2 (1, 2)


    ''
    '' implicit call statement in statement
    ''
    Dim I
    
    I = Module.GetModule().SomeVariable
    
    I = Module.GetModule().Function1
    I = Module.GetModule().Function1()
    '' I = Call Module.GetModule().Function1
    '' I = Call Module.GetModule().Function1()
    
    '' I = Module.GetModule().Function2 1, 2
    I = Module.GetModule().Function2(1, 2)
    
    '' I = Module.GetModule().Sub1 '' function or variable expected
    '' I = Module.GetModule().Sub1() '' function or variable expected
    '' I = Call Module.GetModule().Sub1() '' function or variable expected
    
    '' I = Module.GetModule().Sub2 1, 2 '' function or variable expected
    '' I = Module.GetModule().Sub2(1, 2) '' function or variable expected
    '' I = Call Module.GetModule().Sub2(1, 2) '' function or variable expected
End Sub


Private Sub Main()
    Dim Module As New Module1
    
    TestSimpleCalls
    TestMemberCalls Module
    TestMemberCallsRecursive Module
End Sub

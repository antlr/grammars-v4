#if Debug
	slogLevel = SLogLevel.Verbose
#else
	#if Release
		slogLevel = SLogLevel.Error
	#endif
#endif
#if Debug && !Foo
var x = 1
#endif

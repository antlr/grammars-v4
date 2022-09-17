class AExampleActor : AActor
{
	UPROPERTY()
	bool ExampleUPropertyBool = false;
	
	UPROPERTY()
	float ExampleUPropertyFloat = 10.0f;

	FString ExampleString = "";

	default SomeParentInt = 4;

	UFUNCTION()
	void ExampleUFunction()
	{
		Log("This can be called from blueprint");
	}

	void ExampleMethod()
	{
		Log("This can only be called from Angelscript (And C++ via reflection)");
	}

	UFUNCTION(BlueprintOverride)
	void ExampleUFunctionOverride()
	{
		Log("You can override functions from parent classes and BlueprintImplementableEvents in C++");
	}
};
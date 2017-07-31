ALTER BROKER PRIORITY SimpleContractDefaultPriority  
    FOR CONVERSATION  
    SET (PRIORITY_LEVEL = 3);  
ALTER BROKER PRIORITY SimpleContractPriority
    FOR CONVERSATION
    SET (CONTRACT_NAME = SimpleContractB,
         LOCAL_SERVICE_NAME = TargetServiceB,
         REMOTE_SERVICE_NAME = N'InitiatorServiceB',
         PRIORITY_LEVEL = 8);

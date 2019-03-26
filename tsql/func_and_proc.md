# function_call
function_call should be broken into
* system_function_call
* user_defined_function_call
* method_call (non table valued: https://docs.microsoft.com/en-us/dotnet/api/microsoft.sqlserver.server.sqlmethodattribute?redirectedfrom=MSDN&view=netframework-4.7.2#remarks)
* static_method_call

# func vs sp
* sp: full
* func: '..' schema not allowed, linked server not allowed as inline or table source

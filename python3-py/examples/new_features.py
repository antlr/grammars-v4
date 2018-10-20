def ls(self, msg, match):
    """
    A sample function to test the parsing of ** resolution
    """
    langs = list(map(lambda x: x.lower(), match.group(1).split()))

    bears = client.list.bears.get().json()
    bears = [{**{'name': bear}, **content}
             for bear, content in bears.items()]

# Asyncio example from https://stackabuse.com/python-async-await-tutorial/

import asyncio

async def ping_server(ip):  
    pass

@asyncio.coroutine
def load_file(path):  
    pass

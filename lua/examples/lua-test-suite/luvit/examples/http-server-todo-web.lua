local http = require('http');
local url = require('url');
local items = {};
-- see Node.js_in_action.pdf : page 89 (110 of 417)
http.createServer( function(req, res)
	--p(req)
	if req.url=='/' then
		if req.method=='GET' then
			show(res);
		elseif req.method=='POST' then
			add(req, res);
		else
			badRequest(res);
		end
	else
		notFound(res);
	end
end) :listen(8080)

function map(a, fcn)
	local b={}
	for i,v in ipairs(a) do
		table.insert(b, fcn(v))
	end
	return b
end

function show(res) 
	local html = '<html><head><title>Todo List</title></head><body>\n'
	.. '<h1>Todo List</h1>\n' 		-- For simple apps, inlining
	.. '<ul>\n' 						-- the HTML instead of
	.. table.concat(map(items, function(item) 	--using a template engine works well.
		return '<li>' .. item .. '</li>'
	end),'\n')
	.. '</ul>\n'
	.. '<form method="post" action="/">\n'
	.. '<p><input type="text" name="item" /></p>\n'
	.. '<p><input type="submit" value="Add Item" /></p>\n'
	.. '</form></body></html>\n';
	res:setHeader('Content-Type', 'text/html');
	res:setHeader('Content-Length', #html);
	res:finish(html);
end
function notFound(res) 
	res.statusCode = 404;
	res:setHeader('Content-Type', 'text/plain');
	res:finish('Not Found');
end
function badRequest(res) 
	res.statusCode = 400;
	res:setHeader('Content-Type', 'text/plain');
	res:finish('Bad Request');
end

local qs = require('querystring');
function add(req, res) 
	local body = '';
	--req.setEncoding('utf8');
	req:on('data', function(chunk) body =body.. chunk end);
	req:on('end', function()
		local obj = qs.parse(body);
		--p(obj)
		table.insert(items,obj.item);
		show(res);
	end);
end
print('Server running at http://127.0.0.1:8080/\n Connect to server using a web browser.')

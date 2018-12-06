-- load standard vis module, providing parts of the Lua API
require('vis')
require('python')

vis.events.subscribe(vis.events.INIT, function()
	-- Your global configuration options
	vis:command('set theme phunculist')
end)

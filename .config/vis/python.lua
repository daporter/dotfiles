vis.events.subscribe(vis.events.WIN_OPEN, function(win)
	if win.syntax == "python" then
		vis:command('set autoindent on')
		vis:command('set expandtab on')
		vis:command('set syntax python')
		vis:command('set tabwidth 4')
		vis:map(vis.modes.NORMAL, ' f', function(keys)
			vis:command(',|pyformatter')
		end)
	end
end)

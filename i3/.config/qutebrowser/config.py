# Emacs keybindings
# -----------------

# disable insert mode completely
c.input.insert_mode.auto_enter = False
c.input.insert_mode.auto_leave = False
c.input.insert_mode.plugins = False

# Forward unbound keys
c.input.forward_unbound_keys = "all"

ESC_BIND = 'clear-keychain ;; search ;; fullscreen --leave'

c.bindings.default['normal'] = {}

# Bindings
c.bindings.commands['normal'] = {
    '<ctrl-v>': 'scroll-page 0 0.5',
    '<alt-v>': 'scroll-page 0 -0.5',
    '<ctrl-shift-v>': 'scroll-page 0 1',
    '<alt-shift-v>': 'scroll-page 0 -1',
    '<alt-x>': 'set-cmd-text :',
    '<ctrl-x>b': 'set-cmd-text -s :buffer',
    '<ctrl-x>k': 'tab-close',
    '<ctrl-x><ctrl-c>': 'quit',
    '<ctrl-s>': 'set-cmd-text /',
    '<ctrl-r>': 'set-cmd-text ?',
    '<alt-s>': 'hint all',
    '<ctrl-]>': 'forward',
    '<ctrl-[>': 'back',
    '<ctrl-tab>': 'tab-next',
    '<ctrl-shift-tab>': 'tab-prev',
    '<ctrl-l>': 'set-cmd-text -s :open',
    '<alt-l>': 'set-cmd-text -s :open -t',
    '<ctrl-f>': 'fake-key <Right>',
    '<ctrl-b>': 'fake-key <Left>',
    '<ctrl-a>': 'fake-key <Home>',
    '<ctrl-e>': 'fake-key <End>',
    '<ctrl-n>': 'fake-key <Down>',
    '<ctrl-p>': 'fake-key <Up>',
    '<alt-f>': 'fake-key <Ctrl-Right>',
    '<alt-b>': 'fake-key <Ctrl-Left>',
    '<ctrl-d>': 'fake-key <Delete>',
    '<alt-d>': 'fake-key <Ctrl-Delete>',
    '<alt-backspace>': 'fake-key <Ctrl-Backspace>',
    '<ctrl-w>': 'fake-key <Ctrl-backspace>',
    '<ctrl-y>': 'insert-text {clipboard}',
    '1': 'fake-key 1',
    '2': 'fake-key 2',
    '3': 'fake-key 3',
    '4': 'fake-key 4',
    '5': 'fake-key 5',
    '6': 'fake-key 6',
    '7': 'fake-key 7',
    '8': 'fake-key 8',
    '9': 'fake-key 9',
    '0': 'fake-key 0',
    '<ctrl-h>': 'set-cmd-text -s :help',
    '<ctrl-g>': ESC_BIND,
}

c.bindings.commands['command'] = {
    '<ctrl-s>': 'search-next',
    '<ctrl-r>': 'search-prev',
    '<ctrl-p>': 'completion-item-focus prev',
    '<ctrl-n>': 'completion-item-focus next',
    '<alt-p>': 'command-history-prev',
    '<alt-n>': 'command-history-next',
    '<ctrl-g>': 'leave-mode',
}

c.bindings.commands['hint'] = {
    '<ctrl-g>': 'leave-mode',
}


c.bindings.commands['caret'] = {
    '<ctrl-g>': 'leave-mode',
}

# Search engine
# -------------

c.url.searchengines["DEFAULT"] = "https://www.google.com/search?hl=en&q={}"

# Blank Start page
# ----------------

c.url.default_page = "about:blank"
c.url.start_pages = "about:blank"

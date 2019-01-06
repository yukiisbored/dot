# Yuki's fish configuration
# =========================

# Colored ls
# ----------

switch (uname)
    case OpenBSD
	if command -sq colorls
	    function ls
		command colorls -G
	    end
	end
    case FreeBSD DragonFly
	function ls
	    command ls -G
	end
    case Linux SunOS
	function ls
	    command ls --color
	end
end

# No need to remind that fish is a friendly shell ;)
# --------------------------------------------------

set fish_greeting

# Local bin directory
# -------------------

set -gx PATH $HOME/bin $PATH

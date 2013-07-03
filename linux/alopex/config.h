/* APPEARANCE SETTINGS
 * ---------------------------------------------------------------------------
 */

// An `xfontsel` value of a valid bitmap font to be used on the statusbar.
static const char font[] = "-xos4-terminus-medium-r-normal--13-*-*-*-*-*-iso10646-1";

// Determines the default status, cycle order and availability of the tiling
// modes.
static const char *tile_modes[] = { "monocle", "bstack", "rstack", NULL };

// Determines the appearance of tags on the statusbar.
static const Tagcon tagcons[] = {
	/* prefix icon  suffix */
	{ "1:",   NULL, "one"   },
	{ "2:",   NULL, "two"   },
	{ "3:",   NULL, "three" },
	{ "4:",   NULL, "four"  },
	{ "5:",   NULL, "five"  },
	{ "6:",   NULL, "six"   },
	{ "7:",   NULL, "seven" },
	{ "8:",   NULL, "eight" },
	{ "9:",   NULL, "nine"  },
};

//The x11 cursor designated for alopex to render.
static const char alopex_cursor = XC_left_ptr;

// Width of a border to be drawn around each window.
static const int borderwidth = 1;

// Set gap between the windows and the statusbar.
static const int tilegap = 0;

// Minimum space between tags and tabs.
static const int tagspace = 0;

// Maximum lenght of input you may feed to the statusline's stdin reader.
static const int max_status_line = 256;

// The smallest a window can get in height/width.
static const int win_min = 20;

// If hovering over a window should change focus to it.
static const Bool focusfollowmouse = True;

// Whether the statusbar should be displayed.
static Bool showbar = True;

// Whether to display the statusbar on top (true) or bottom (false).
static Bool topbar = True;

// The advantage in pixels the main window should have over the other windows.
static int tilebias = 0;

// Where new windows will be placed when created. 0 master section; 1 top of
// the stack; 2 bottom of the stack.
static const int attachmode = 1;

// How many windows can be visible in the stack region at any given time.
static int stackcount = 3;


/* BEHAVIORAL ARRAYS
 * ---------------------------------------------------------------------------
 */



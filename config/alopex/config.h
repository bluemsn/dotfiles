/* APPEARANCE SETTINGS
 * ---------------------------------------------------------------------------
 */

// An `xfontsel` value of a valid bitmap font to be used on the statusbar.
static const char font[] = "-misc-fixed-medium-r-normal--13-120-75-75-c-70-*-*";

// Determines the default status, cycle order and availability of the tiling
// modes.
static const char *tile_modes[] = { "monocle", "bstack", "rstack", NULL };

// Determines the appearance of tags on the statusbar.
static const Tagcon tagcons[] = {
	/* prefix icon     suffix */
	{ NULL,   NO_ICON, "1" },
	{ NULL,   NO_ICON, "2" },
	{ NULL,   NO_ICON, "3" },
	{ NULL,   NO_ICON, "4" },
	{ NULL,   NO_ICON, "5" },
	{ NULL,   NO_ICON, "6" },
	{ NULL,   NO_ICON, "7" },
	{ NULL,   NO_ICON, "8" },
	{ NULL,   NO_ICON, "9" },
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

/* VARIABLES AND STUFF */
#define DMENU "dmenu_run -fn \"-misc-fixed-medium-r-normal--13-120-75-75-c-70-*-*\" -nb \"#101010\" -nf \"#484862\" -sb \"#080808\" -sf \"#FFDD0E\""

#define TERM "terminator" // Set the terminal to use

#define CMD(app) app "&"

#define XRANDR_CMD "xrandr --output LVDS1 --auto --output VGA1 --auto --left-of LVDS1"
//#define WALLPAPER "feh --bg-scale ~/dotfiles/alopex/bg.jpg"

/* KEYIFIER KEYS */
#define KEY1 Mod4Mask
#define KEY2 Mod1Mask
#define KEY3 ControlMask
#define KEY4 ShiftMask
// I've no idea what this does
#define TagKey(KEY,TAG) \
{ KEY1,      KEY, tag, "s " TAG }, \
{ KEY1|KEY2, KEY, tag, "t " TAG }, \
{ KEY1|KEY3, KEY, tag, "m " TAG }, \
{ KEY1|KEY4, KEY, tag, "a " TAG },

/* KEYBINDINGS */
static Key keys[] = {
	// Launchers + Misc.
	{ KEY1,      XK_Return,  spawn,      CMD(TERM)     }, // Launch terminal
	{ KEY1,      XK_p,       spawn,      CMD(DMENU)    }, // Use Dmenu
	//{ KEY1,      XK_w,       spawn,      CMD("chromium") }, // Launch web browser
	{ KEY1|KEY4, XK_q,       quit,       NULL          }, // Close Alopex
	{ KEY2,      XK_F4,      killclient, NULL          }, // Close window
	{ KEY1,      XK_f,       fullscreen, NULL          }, // Toggle fullscreen mode
	{ KEY1,      XK_Menu,    windowlist, "interrobang alopex" }, // App jump with Interrobang thing
	{ KEY1|KEY2, XK_f,       toggle,     "floating"    }, // Toggle between floating window and not
	{ KEY1,      XK_x,       toggle,     "place bar"   }, // Change statusbar to top/bottom
	{ KEY1,      XK_a,       toggle,     "visible bar" }, // Show/hide the statusbar
	{ 0,         0x1008ff59, spawn,      XRANDR_CMD    }, // No idea...

	// Tiling management
	{ KEY1, XK_space,  tile,      "cycle"    }, // Cycle between window layouts
	{ KEY1, XK_i,      tile_conf, "increase" }, // Increase pixels alloted to current window
	{ KEY1, XK_d,      tile_conf, "decrease" }, // Decrease pixels alloted to current window
	{ KEY1, XK_equal,  tile_conf, "+"        }, // Not sure...
	{ KEY1, XK_minus,  tile_conf, "-"        }, // Not sure...
	{ KEY1, XK_period, tile_conf, "all"      }, // Not sure...
	{ KEY1, XK_comma,  tile_conf, "one"      }, // Not sure...

	// Tagging
	{ KEY2, XK_Tab, tag, "flip" }, // Doing this seems to flip the side of the tag so to speak...
	TagKey(XK_1, "1") // Switch monitor to tag 1, 2, 3 etc.
	TagKey(XK_2, "2")
	TagKey(XK_3, "3")
	TagKey(XK_4, "4")
	TagKey(XK_5, "5")
	TagKey(XK_6, "6")
	TagKey(XK_7, "7")
	TagKey(XK_8, "8")
	TagKey(XK_9, "9")

	// Window focus/movement
	// f  = Focus previous, next, or alternate window in tag(s)
	// s  = Swap window with previous, next, or alternate window
	// capital "Next" or "Prev" = Include floating windows
	// +/- = Adjust a windows monitor number
	{ KEY1,      XK_k,     window, "f prev"        },
	{ KEY1,      XK_j,     window, "f next"        },
	{ KEY1|KEY4, XK_k,     window, "f Prev"        },
	{ KEY1|KEY4, XK_j,     window, "f Next"        },
	{ KEY1,      XK_Left,  window, "f prev"        },
	{ KEY1,      XK_Right, window, "f next"        },
	{ KEY1,      XK_h,     window, "s prev"        },
	{ KEY1,      XK_l,     window, "s next"        },
	{ KEY1,      XK_Up,    window, "s prev"        },
	{ KEY1,      XK_Down,  window, "s next"        },
	{ KEY1,      XK_Tab,   window, "f alt"         },
	{ KEY1|KEY4, XK_Tab,   window, "s alt"         },
	{ KEY1|KEY4, XK_equal, window, "+"             },
	{ KEY1|KEY4, XK_minus, window, "-"             },
	{ KEY1|KEY4, XK_space, toggle, "monitor focus" },
};

static Button buttons[] = {
	{ KEY1, 1, mouse,  "move"     },
	{ KEY1, 2, toggle, "floating" },
	{ KEY1, 3, mouse,  "resize"   },
	{ KEY1, 4, window, "s prev"   },
	{ KEY1, 5, window, "s next"   },
	{ KEY1, 6, window, "f prev"   },
	{ KEY1, 7, window, "f next"   },
};

static Rule rules[] = {
	// Name class tags flags
	// Tags/flags: 0 means no change to default
	{ NULL,    "float", 0, FLAG_FLOATING },
	{ "float", NULL,    0, FLAG_FLOATING },
};

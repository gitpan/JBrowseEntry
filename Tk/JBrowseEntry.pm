#
# JBrowseEntry is a stripped down version of ComboBox.tcl from Tix4.0

=head1 NAME

JBrowseEntry is a full-featured "Combo-box" (Text-entry combined with drop-down 
listbox.

=head1 DESCRIPTION

JBrowseEntry widgets allow one to specify a full combo-box, a "readonly" 
box (text field allows user to type the 1st letter of an item to search for, 
but user may only ultimately select one of the items in the list), or a 
"textonly" version (drop-down list disabled), or a completely disabled 
widget.  One may also specify whether or not the button which activates the 
dropdown list via the mouse can take focus or not (-btntakesfocus) or 
whether the widget itself can take focus or is skipped in the focusing 
order.  The developer can also specify the maximum length of the dropdown 
list such that if more than that number of items is added, a scrollbar is 
automatically added (-height).

this widget is similar to other combo-boxes, ie. 	JComboBox, but has good 
keyboard bindings and allows for quick lookup/search within the listbox. 
pressing <RETURN> or <DOWN> in entry field displays the dropdown box with the 
first entry most closly matching whatever's in the entry field highlighted. 
<UP> and <DOWN> arrows work the listbox as well as pressing a key, which will 
move the highlight to the next item starting with that letter/number, etc.  
pressing <RETURN> in the listbox selects the highlighted entry and copies it 
to the text field and removes the listbox.  <SPACE> copies highlighted item 
to the text field but leaves listbox showing and focused.  <ESC> removes the 
listbox from view.  

=head1 EXAMPLES

 It is easiest to illustrate this widget's capabilities via examples:
 
 use Tk;
 use Tk::JBrowseEntry;
 
 $MainWin = MainWindow->new;
 
 #SET UP SOME DEFAULT VALUES.
 
 $dbname1 = 'cows';
 $dbname2 = 'foxes';
 $dbname3 = 'goats';
 $dbname5 = 'default';
 
 #HERE'S A NORMAL COMBO-BOX.
 
 $jb1 = $MainWin->JBrowseEntry(
 	-label => 'Normal:',
 	-variable => \$dbname1,
 	-state => 'normal',
 	-choices => [qw(pigs cows foxes goats)],
 	-width  => 12);
 $jb1->pack(
 	-side   => 'top', -pady => '10', -anchor => 'w');
 
 #THIS ONE HAS THE DROPDOWN LIST DISABLED.
 
 $jb2 = $MainWin->JBrowseEntry(
 	-label => 'TextOnly:',
 	-variable => \$dbname2,
 	-state => 'textonly',
 	-choices => [qw(pigs cows foxes goats)],
 	-width  => 12);
 $jb2->pack(
 	-side   => 'top', -pady => '10', -anchor => 'w');
 
 #THIS ONE'S "READONLY" (USER MUST PICK FROM THE LIST, TEXT BOX ALLOWS QUICK 
 #SEARCH.
 
 $jb3 = $MainWin->JBrowseEntry(
 	-label => 'ReadOnly:',
 	-variable => \$dbname3,
 	-choices => [qw(pigs cows foxes goats)],
 	-state => 'readonly',
 	-width  => 12);
 $jb3->pack(
 	-side   => 'top', -pady => '10', -anchor => 'w');
 
 #THIS ONE'S COMPLETELY DISABLED!
 
 $jb4 = $MainWin->JBrowseEntry(
 	-label => 'Disabled:',
 	-variable => \$dbname3,
 	-state => 'disabled',
 	-choices => [qw(pigs cows foxes goats)],
 	-width  => 12);
 $jb4->pack(
 	-side   => 'top', -pady => '10', -anchor => 'w');
 
 #HERE'S ONE WITH A SCROLLBAR (NOTE THE "-height" ATTRIBUTE).
 
 $jb5 = $MainWin->JBrowseEntry(
 	-label => 'Scrolled List:',
 	-width => 12,
 	-default => $dbname5,
 	-height => 4,
 	-variable => \$dbname5,
 	-browsecmd => sub {print "-browsecmd!\n";},
 	-listcmd => sub {print "-listcmd!\n";},
 	-state => 'normal',
 	-choices => [qw(pigs cows foxes goats horses sheep dogs cats ardvarks default)]);
 $jb5->pack(
 	-side   => 'top', -pady => '10', -anchor => 'w');
 
 #HERE'S ONE THAT THE BUTTON TAKES KEYBOARD FOCUS.
 
 $jb6 = $MainWin->JBrowseEntry(
 	-label => 'Button Focus:',
 	-btntakesfocus => 1,
 	-arrowimage => $MainWin->Getimage('balArrow'),   #SPECIFY A DIFFERENT BUTTON IMAGE.
 	-farrowimage => $MainWin->Getimage('cbxarrow'),  #OPTIONAL 2ND IMAGE FOR BUTTON WHEN FOCUSED. 
 	-width => 12,
 	-height => 4,
 	-variable => \$dbname6,
 	-browsecmd => sub {print "-browsecmd!\n";},
 	-listcmd => sub {print "-listcmd!\n";},
 	-state => 'normal',
 	-choices => [qw(pigs cows foxes goats horses sheep dogs cats ardvarks default)]);
 $jb6->pack(
 	-side   => 'top', -pady => '10', -anchor => 'w');
 
 #HERE'S ONE THAT DOWS NOT TAKE KEYBOARD FOCUS.
 
 $jb7 = $MainWin->JBrowseEntry(
 	-label => 'Skip Focus:',
 	-takefocus => 0,
 	-width => 12,
 	-height => 4,
 	-variable => \$dbname7,
 	-browsecmd => sub {print "-browsecmd!\n";},
 	-listcmd => sub {print "-listcmd!\n";},
 	-state => 'normal',
 	-choices => [qw(pigs cows foxes goats horses sheep dogs cats ardvarks default)]);
 $jb7->pack(
 	-side   => 'top', -pady => '10', -anchor => 'w');
 
 $jb7->choices([qw(First Second Fifth Sixth)]);   #REPLACE LIST CHOICES!
 $jb7->insert(2, 'Third', 'Fourth');              #ADD MORE AFTER 1ST 2.
 $jb7->insert('end', [qw(Seventh Oops Eighth)]);  #ADD STILL MORE AT END.
 $jb7->delete(7);                                 #REMOVE ONE.
 
 $b = $MainWin->Button(-text => 'Quit', -command => sub {exit(); });
 $b->pack(-side => 'top');
 $jb1->focus;   #PICK ONE TO START WITH KEYBOARD FOCUS.
 
 MainLoop;

=cut

package Tk::JBrowseEntry;

use vars qw($VERSION);
$VERSION = '4.2'; # $Id: //depot/Tk8/Tixish/JBrowseEntry.pm#14$

use Tk;
use Carp;
use strict;

require Tk::Frame;
require Tk::LabEntry;

use base qw(Tk::Frame);
Construct Tk::Widget 'JBrowseEntry';

my ($BITMAP, $FOCUSEDBITMAP);

sub ClassInit
{
	my($class,$mw) = @_;

	unless(defined($BITMAP))
	{
		$BITMAP = __PACKAGE__ . "::downarrwow";

		if ($Tk::platform =~ /Win32/)
		{
			my $bits = pack("b10"x10,
					"..........",
					"..........",
					"..........",
					".#########",
					"..#######.",
					"...#####..",
					"....###...",
					".....#....",
					"..........",
					".........."
					);
			$mw->DefineBitmap($BITMAP => 10,10, $bits);
		}
		else
		{
			my $bits = pack("b11"x12,
					"....###....",
					"....###....",
					"....###....",
					"....###....",
					".#########.",
					"..#######..",
					"...#####...",
					"....###....",
					".....#.....",
					"...........",
					".#########.",
					".#########."
			);
			$mw->DefineBitmap($BITMAP => 11,12, $bits);
		}
		$FOCUSEDBITMAP = __PACKAGE__ . "::fdownarrow";

		if ($Tk::platform =~ /Win32/)
		{
			my $bits = pack("b10"x10,
					".#.#.#.#.#",
					".#.......#",
					"..........",
					".#########",
					"..#######.",
					".#.#####.#",
					"....###...",
					".#...#...#",
					"..........",
					".##.#.#.##"
			);
			$mw->DefineBitmap($FOCUSEDBITMAP => 10,10, $bits);
		}
	}
}

sub Populate
{
	my ($w, $args) = @_;
#print "-args=".join('|',@_);
#foreach (keys %{$_[1]}) {print "-value($_)=$_[1]->{$_}=\n";};
#print "-default=$_[1]->{'-default'}=\n";
	$w->{btntakesfocus} = 0;
	$w->{btntakesfocus} = delete ($args->{-btntakesfocus})  if (defined($args->{-btntakesfocus}));
	$w->{arrowimage} = $args->{-arrowimage}  if (defined($args->{-arrowimage}));
	$w->{farrowimage} = delete ($args->{-farrowimage})  if (defined($args->{-farrowimage}));
	$w->{arrowimage} ||= $w->{farrowimage}  if ($w->{farrowimage});
	$w->{takefocus} = 1;
	$w->{takefocus} = delete ($args->{-takefocus})  if (defined($args->{-takefocus}));
	$w->{-listwidth} = $args->{-width}  if (defined($args->{-width}));
	$w->{-foreground} = $args->{-foreground}  if (defined($args->{-foreground}));
	$w->{-background} = $args->{-background}  if (defined($args->{-background}));
	$w->{-borderwidth} = delete($args->{-borderwidth})  if (defined($args->{-borderwidth}));
	$w->{-relief} = delete($args->{-relief})  if (defined($args->{-relief}));
	$w->SUPER::Populate($args);

	# ENTRY WIDGET AND ARROW BUTTON

	my $lpack = delete $args->{-labelPack};
	unless (defined $lpack)
	{
		$lpack = [-side => "left", -anchor => "e"];
	}
	my $labelvalue = $args->{-label};
#foreach my $x (%$args) {print "-arg($x)=$args->{$x}=\n";};
	my $ll = $w->Label(-text => delete $args->{-label});
	my $tf = $w->Frame(-borderwidth => ($w->{-borderwidth} || 2), -highlightthickness => 1, 
			-relief => ($w->{-relief} || 'sunken'));
	my $e = $tf->LabEntry(-borderwidth => 0, -relief => 'flat');
	my $b = $tf->Button(-borderwidth => 1, -takefocus => $w->{btntakesfocus}, 
			#-bitmap => '@' . Tk->findINC("balArrow.xbm"));
			-bitmap => $BITMAP);
			#-bitmap => '@' . Tk->findINC("cbxarrow.xbm"));
	#$ll->packForget()  unless ($labelvalue);
	if ($labelvalue)
	{
		#$ll->pack(-side => 'left');
		$ll->pack(@$lpack);
	}
	else
	{
		$ll->packForget();
	}
	$w->Advertise("entry" => $e);
	$w->Advertise("arrow" => $b);
	$w->Advertise("frame" => $tf);
	my ($ee) = $w->Subwidget("entry");
	$w->Advertise("textpart" => $ee);
	$tf->pack(-side => "right", -padx => 0, -pady => 0, -fill => 'x', -expand => 1);
	$b->pack(-side => "right", -padx => 0, -pady => 0);
	$e->pack(-side => "right", -fill => 'x', -padx => 0, -pady => 0, -expand => 1); #, -padx => 1);

    # popup shell for listbox with values.
	my $c = $w->Toplevel(-bd => 2, -relief => "raised");
	$c->overrideredirect(1);
	$c->withdraw;
	my $sl = $c->Scrolled( qw/Listbox -selectmode browse -scrollbars oe/ );
	if (defined($w->{-foreground}))
	{
		$e->configure(-foreground => $w->{-foreground}, -highlightcolor => $w->{-foreground});
		$tf->configure(-foreground => $w->{-foreground});
		$sl->configure(-foreground => $w->{-foreground}, -highlightcolor => $w->{-foreground});
		$b->configure(-foreground => $w->{-foreground});
	}
	if (defined($w->{-background}))
	{
		$e->configure(-background => $w->{-background}, -highlightbackground => $w->{-background});
		$tf->configure(-background => $w->{-background}, -highlightbackground => $w->{-background});
		$sl->configure(-background => $w->{-background}, -highlightbackground => $w->{-background});
		$b->configure(-background => $w->{-background}, -highlightbackground => $w->{-background});
	}
	$w->Advertise("choices" => $c);
	$w->Advertise("slistbox" => $sl);
	$sl->pack(-expand => 1, -fill => "both");

    # other initializations
	$w->SetBindings;
	$w->{"popped"} = 0;
	$w->Delegates('insert' => $sl, 'delete' => $sl, get => $sl, DEFAULT => $e);
	$w->ConfigSpecs(
			-listwidth   => [qw/PASSIVE  listWidth   ListWidth/,   undef],
			-height      => [qw/PASSIVE  height      Height/,      undef],
			-listcmd     => [qw/CALLBACK listCmd     ListCmd/,     undef],
			-browsecmd   => [qw/CALLBACK browseCmd   BrowseCmd/,   undef],
			-choices     => [qw/METHOD   choices     Choices/,     undef],
			-state       => [qw/METHOD   state       State         normal/],
			-arrowimage  => [ {-image => $b}, qw/arrowImage ArrowImage/, undef],
			-variable    => "-textvariable",
			-colorstate  => [qw/PASSIVE  colorState  ColorState/,  undef],
			-default   => "-textvariable", 
			-imgname   => '',
			-img0      => '',
			-img1      => '',
	DEFAULT      => [$e] );

	my $var_ref = $w->cget( "-textvariable" );
	unless (defined($var_ref) && ref($var_ref))
	{
		$var_ref = '';
		$w->configure(-textvariable => \$var_ref);
	}

	eval { $w->{'default'} = $_[1]->{'-default'} || ${$_[1]->{-variable}}; };  #JWT


#print "??? w=$w=\n";
#foreach (keys %{$w->{ConfigSpecs}}) {print "-value($_)=$w->{ConfigSpecs}->{$_}=\n";};
}

sub focus
{
	my ($w) = shift;
	my ($state) = $w->cget( "-state" );
#print "-focus! state=$state= tf=$w->{takefocus}=\n";

	if ($state eq 'disabled')
	{
		eval {$w->focusNext->focus; };
	}
	else
	{
		if ($state eq 'readonly')
		{
			$w->Subwidget("frame")->focus;
		}
		else
		{
			$w->Subwidget("entry")->focus;
		}
		$w->Subwidget("arrow")->focus  if ($state eq 'normal' && !$w->{takefocus} && $w->{btntakesfocus});
		$w->Subwidget("entry")->selectionRange(0,'end'); #  unless ($state eq 'readonly' && $w->{btntakesfocus});
	}
}

sub SetBindings
{
	my ($w) = @_;

	my $e = $w->Subwidget("entry");
	my $f = $w->Subwidget("frame");
	my $b = $w->Subwidget("arrow");
	my $sl = $w->Subwidget("slistbox");
	my $l = $sl->Subwidget("listbox");

	local *returnFn = sub
	{
		my ($state) = $w->cget( "-state" );
	
		&LbFindSelection($w);
		unless ($w->{"popped"})
		{
			$w->BtnDown;
			return if ($state eq 'textonly' || $state eq 'disabled');
			$w->{'savefocus'} = $w->focusCurrent;
			$w->Subwidget("slistbox")->focus;
		}
		else
		{
			$w->LbCopySelection;
			$e->selectionRange(0,'end');
			Tk->break;
		}
	};

	local *downFn = sub
	{
		my ($state) = $w->cget( "-state" );
		&LbFindSelection($w);         #JWT
		if ($w->{"popped"})
		{
			return if ($state eq 'textonly' || $state eq 'disabled');
			&LbFindSelection($w);         #JWT
			$w->{'savefocus'} = $w->focusCurrent;
			$w->Subwidget("slistbox")->focus;
		}
		else
		{
			&LbFindSelection($w);         #JWT
			my $l = $w->Subwidget("slistbox")->Subwidget("listbox");
			my (@listsels) = $l->get('0','end');
			my $index = $w->LbIndex;
			if (&LbFindSelection($w) == 1)
			{
				$index += 1;
				$index = 0  if ($index > $#listsels);
			}
			my $var_ref = $w->cget( "-textvariable" );
			$$var_ref = $listsels[$index];
		}
	};
	
	local *upFn = sub
	{
		my ($state) = $w->cget( "-state" );
		if ($w->{"popped"})
		{
			return if ($state eq 'textonly' || $state eq 'disabled');
			&LbFindSelection($w);
			$w->{'savefocus'} = $w->focusCurrent;
			$w->Subwidget("slistbox")->focus;
		}
		else
		{
			&LbFindSelection($w);
			my $l = $w->Subwidget("slistbox")->Subwidget("listbox");
			my (@listsels) = $l->get('0','end');
			my $index = $w->LbIndex - 1;
			$index = $#listsels  if ($index < 0);
			my $var_ref = $w->cget( "-textvariable" );
			$$var_ref = $listsels[$index];
		}
	};

	local *escapeFn = sub
	{
		if ($w->{"popped"})
		{
			$w->Popdown;
		}
		else
		{
			my $var_ref = $w->cget( "-textvariable" );
			if ($$var_ref eq $w->{'default'} && $w->cget( "-state" ) ne "readonly")
			{
				$$var_ref = '';
			}
			else
			{
				$$var_ref = $w->{'default'};
			}
		}
		$e->selectionRange(0,'end');   #ADDED 20020716
		Tk->break;
	};

	# SET BIND TAGS

	$w->bindtags([$w, 'Tk::JBrowseEntry', $w->toplevel, "all"]);
	$e->bindtags([$e, $e->toplevel, "all"]);

	# BINDINGS FOR THE BUTTON AND ENTRY

	$b->bind("<FocusIn>", sub
	{
		$b = shift;
		my $img = $w->{farrowimage} || $b->cget('-image');
		if ($img)
		{
			unless ($w->{img0})
			{
				$w->{img0} = $img;
			}
			$b->configure(-image => $w->{img0});					
		}
		elsif ($^O =~ /Win/i)
		{
			$w->{img0} = $FOCUSEDBITMAP;
			$b->configure(-bitmap => $w->{img0});
		}
		$w->{imgname} = 'cbfarrow';
	}
	);
	$b->bind("<FocusOut>", sub
	{
		$b = shift;
		my $img = $w->{arrowimage} || $b->cget('-image');
		if ($img)
		{
			unless ($w->{img1})
			{
				$w->{img1} = $img;
			}
			$b->configure(-image => $w->{img1});					
		}
		elsif ($^O =~ /Win/i)
		{
			$w->{img1} = $BITMAP;
			$b->configure(-bitmap => $w->{img1});
		}
		$w->{imgname} = 'cbxarrow';
	}
	);

	$b->bind('<1>', sub
	{
		my ($state) = $w->cget( "-state" );
		&LbFindSelection($w);         #JWT
		$w->BtnDown;    #POPS UP LISTBOX!
		if ($state eq 'readonly')
		{
	     		$w->{'savefocus'} = $w->focusCurrent;
 	    		$w->Subwidget("slistbox")->focus;
		}
		else
		{
			$e->focus;
		}
		Tk->break;
	}
	);
	$b->toplevel->bind("<ButtonRelease-1>", sub {$w->ButtonHack;});
#$e->bind("<ButtonRelease-1>", sub {print "-ebind!\n"; $w->LbClose;});
#JWT: UNCOMMENTING ABOVE LINE WILL UNPOST LISTBOX IF MOUSE CLICKED IN TEXTAREA, 
#BUT WILL ELIMINATE ABILITY TO SELECT, TYPE, AND HAVE SELECTED MENU ITEM CHANGE 
#WHILE TYPING!!!

	$b->bind("<space>", sub
	{
		my ($state) = $w->cget( "-state" );
		return if ($state eq 'textonly' || $state eq 'disabled');

		&LbFindSelection($w);
		$w->BtnDown;
		$w->{'savefocus'} = $w->focusCurrent;
		$w->Subwidget("slistbox")->focus;
	}
	);
	$b->bind("<Return>", sub
	{
		my ($state) = $w->cget( "-state" );
		return if ($state eq 'textonly' || $state eq 'disabled');

		&LbFindSelection($w);
		$w->BtnDown;
		$w->{'savefocus'} = $w->focusCurrent;
		$w->Subwidget("slistbox")->focus;
		Tk->break;
	}
	);

	$b->bind("<Down>", sub
	{
		my ($state) = $w->cget( "-state" );
		return if ($state eq 'textonly' || $state eq 'disabled');

		&LbFindSelection($w);
		$w->BtnDown;
		$w->{'savefocus'} = $w->focusCurrent;
		$w->Subwidget("slistbox")->focus;
		Tk->break;
	}
	);

	$e->bind("<Return>", \&returnFn);
	$f->bind("<Return>", \&returnFn);

	$e->bind("<Down>", \&downFn);
	$f->bind("<Down>", \&downFn);

	$e->bind("<Up>", \&upFn);
	$f->bind("<Up>", \&upFn);

	$e->bind('<Escape>' => \&escapeFn);
	$f->bind('<Escape>' => \&escapeFn);

	$e->bind("<Left>", sub {Tk->break;});
	$e->bind("<Right>", sub {Tk->break;});
	$f->bind("<Left>", sub {Tk->break;});
	$f->bind("<Right>", sub {Tk->break;});
	$e->bind("<Tab>", sub
	{
		$w->Popdown  if  ($w->{"popped"});   #UNDISPLAYS LISTBOX.
		eval { shift->focusNext->focus; };
	}
	);
	$f->bind("<Tab>", sub
	{
		$w->Popdown  if  ($w->{"popped"});
		eval { shift->focusNext->focus; };
	}
	);

	# bindings for listbox

	$l->configure(-selectmode => 'browse');
	$l->configure(-takefocus => 1);   #JWT
	$l->bind("<ButtonRelease-1>", sub
	{
		$w->ButtonHack;
		LbChoose($w, $l->XEvent->x, $l->XEvent->y);
	}
	);
	$l->bind('<Escape>' => sub
	{
		$w->LbClose;
		Tk->break;
	}
	);
	$l->bind('<Return>' => sub
	{
		$w->LbCopySelection;
		$e->selectionRange(0,'end');
		Tk->break;
	}
	);
	$l->bind('<space>' => sub
	{
		my ($state) = $w->cget( "-state" );
		$w->LbCopySelection;
		$e->selectionRange(0,'end');
		Tk->break;
	}
	);

	$l->bind('<Tab>' => sub
	{
		my ($state) = $w->cget( "-state" );
		$w->Popdown  if ($^O !~ /Win/i && !$w->{takefocus});  #WINDUHS LOWERS LISTBOX BEHIND CALLER (HIDES IT)!
		$w->Popdown  if ($^O =~ /Win/i || $state eq 'readonly');  #WINDUHS LOWERS LISTBOX BEHIND CALLER (HIDES IT)!
		$e->focus()  unless ($state eq 'readonly');  #SO WE'LL POP IT DOWN FIRST! (RAISE WOULDN'T WORK :-()
		$w->BtnDown  if ($^O =~ /Win/i && $state ne 'readonly' && $w->{takefocus});
		Tk->break;
	}
	);
	$l->bind('<Key>' => [\&keyFn,$w,$e,$l,1]);  
			#if $w->cget( "-state" ) eq "readonly";    #JWT
	$e->bind('<Key>' => [\&keyFn,$w,$e,$l]);
	$f->bind('<Key>' => [\&keyFn,$w,$e,$l]);
			#unless $w->cget( "-state" ) eq "readonly";    #JWT
	$e->bind('<1>' => sub { 
		my ($state) = $w->cget( "-state" );
		if ($state eq 'readonly')
		{
			&LbFindSelection($w);         #JWT
			$w->BtnDown;
	     		$w->{'savefocus'} = $w->focusCurrent;
 	    		$w->Subwidget("slistbox")->focus;
 	    		Tk->break;
		}
		else
		{
			$e->focus;
 	    		Tk->break;
		}
	});

	# allow click outside the popped up listbox to pop it down.

	$w->bind("<1>", sub {$w->BtnDown; Tk->break});
	$w->parent->bind("<1>", sub
	{
		if ($w->{"popped"})
		{
			$w->Popdown(1);
		}
	}
	);
	$w->bind("<FocusIn>", \&focus);
	$w->bind('<Alt-f>', sub {print "-focus=".$w->focusCurrent()."=\n";});
	$w->bind('<ButtonRelease-2>', sub {print "-focus=".$w->focusCurrent()."=\n";});
}

sub keyFn    #JWT: TRAP LETTERS PRESSED AND ADJUST SELECTION ACCORDINGLY.
{
#print "-keyfn: parms=".join('|',@_)."=\n";
	my ($x,$w,$e,$l,$flag) = @_;
#print "-keyfn($x,$w,$e,$l,$flag)\n";
	my $mykey = $x->XEvent->A;
#print "-keyFn: pressed=$mykey=\n";
	if ($w->cget( "-state" ) eq "readonly")  #ADDED 20020711 TO ALLOW TYPING 1ST LETTER TO SELECT NEXT VALID ITEM!
	{
		&LbFindSelection($w,$mykey)  if ($mykey);  #JUMP TO 1ST ITEM STARTING WITH THIS KEY
		$w->LbCopySelection(1);
#		unless ($w->{"popped"})   #THIS WILL CAUSE LISTBOX TO BE DISPLAYED AS WELL.
#		{
#			$w->BtnDown;
#		}
#		$w->{'savefocus'} = $w->focusCurrent;
#		$w->Subwidget("slistbox")->focus;

		$w->Subwidget("entry")->selectionRange(0,'end')  unless ($w->{"popped"});;
	}
	elsif ($flag == 1)      #LISTBOX HAS FOCUS.
	{
		&LbFindSelection($w,$mykey)  if ($mykey);  #JUMP TO 1ST ITEM STARTING WITH THIS KEY
	}
	else  #TEXT FIELD HAS FOCUS.
	{
		&LbFindSelection($w)  if ($mykey);  #JUMP TO 1ST ITEM MATCHING TEXT FIELD.
	}

}

sub BtnDown
{
	my ($w) = @_;
	my ($state) = $w->cget( "-state" );

	return if ($state eq 'textonly' || $state eq 'disabled');

	#JWT NEXT 2 LINES PREVENT POPPING EMPTY LIST!
	my $l = $w->Subwidget("slistbox")->Subwidget("listbox");
	return  unless ($l->get('0','end'));

	if ($w->{"popped"})
	{
		$w->Popdown(1);
		$w->{"buttonHack"} = 0;
	}
	else
	{
		$w->PopupChoices;
		$w->{"buttonHack"} = 1;
	}
}

sub PopupChoices
{
	my ($w) = @_;

	my $first;

	if (!$w->{"popped"})
	{
		$w->Callback(-listcmd, $w);
		my $e = $w->Subwidget("entry");
		my $c = $w->Subwidget("choices");
		my $s = $w->Subwidget("slistbox");
		my $a = $w->Subwidget("arrow");

		my $wheight = $w->cget("-height");
		my (@hh);
		$hh[0]=$w->height;
		$hh[1]=$w->reqheight;
		$hh[2]=$e->height;
		$hh[3]=$e->reqheight;
		$hh[4]=$c->height;
		$hh[5]=$c->reqheight;
		$hh[6]=$s->height;
		$hh[7]=$s->reqheight;
		my $sll = $s->Subwidget("listbox");
		#my $rw = $c->reqwidth;
		my $rw = $c->width;       #JWT
		$first = 1  if ($rw <= 1);
		my ($itemcnt) = $sll->index('end');
		$wheight = 10  unless ($wheight);
		$wheight = $itemcnt  if ($itemcnt < $wheight);
		$wheight = $itemcnt  unless ($wheight);
		$wheight = $itemcnt  unless ($wheight || $itemcnt > 10);
		if ($wheight)
		{
			$sll->configure(-height => ($wheight * $w->height));
			$w->update;
		}

		my $y1 = $e->rooty + $e->height + 3;
		my $bd = $c->cget(-bd) + $c->cget(-highlightthickness);
		my ($unitpixels, $ht, $x1, $ee, $width, $x2);
		if ($^O =~ /Win/i)
		{
			$unitpixels = $e->height - ((2 * $bd) + 1) + 2 + 4;
			#$ht = $s->reqheight + 2 * $bd;
			$ht = ($wheight * $unitpixels) + 10;
#print "-bd=$bd= uht=$unitpixels= ht=$ht= wheight=".$w->cget("-height")."=\n";
			#$x1 = $e->rootx;
			$ee = $w->Subwidget("textpart");   #JWT
			$x1 = $ee->rootx;   #JWT
			$x2 = $a->rootx + $a->width;
			$width = $x2 - $x1;
			$rw = $width + 5;
		}
		else
		{
			$unitpixels = $e->height - ((2 * $bd) + 1) + 4;
			$ht = ($wheight * $unitpixels) + 10;
			#$x1 = $e->rootx;
			$ee = $w->Subwidget("textpart");   #JWT
			$x1 = $ee->rootx;   #JWT
			$x2 = $a->rootx + $a->width;
			$width = $x2 - $x1;
			if ($rw < $width)
			{
				$rw = $width;
			}
			else
			{
				$rw = $width * 3  if ($rw > $width * 3);
				$rw = $w->vrootwidth  if ($rw > $w->vrootwidth);
			}
			$width = $rw;
			if ($first)
			{
				$rw += 6;
				$first = 0;
			}
	# if listbox is too far right, pull it back to the left
	#
			if ($x2 > $w->vrootwidth)
			{
				$x1 = $w->vrootwidth - $width;
			}
		}
#print "-apply fudge factor first=$first=\n";
$x1 -= 3;  #FUDGE TO MAKE DROPDOWN LINE UP VERTICALLY W/ENTRY&BUTTON.

	# if listbox is too far left, pull it back to the right
	#
		if ($x1 < 0)
		{
			$x1 = 0;
		}

	# if listbox is below bottom of screen, pull it up.
		my $y2 = $y1 + $ht;
		if ($y2 > $w->vrootheight)
		{
			$y1 = $y1 - $ht - ($e->height - 5);
		}
		$c->geometry(sprintf("%dx%d+%d+%d", $rw, $ht, $x1, $y1));
		$c->deiconify;
		$c->raise;
	#$e->focus;
		$w->focus;
		$w->{"popped"} = 1;

		&LbFindSelection;                      #JWT
		$c->configure(-cursor => "arrow");
		$w->grabGlobal;
	}
}

# choose value from listbox if appropriate
sub LbChoose
{
	my ($w, $x, $y) = @_;
	my $l = $w->Subwidget("slistbox")->Subwidget("listbox");
	$l->configure(-selectmode => 'browse');
	if ((($x < 0) || ($x > $l->Width)) ||
			(($y < 0) || ($y > $l->Height)))
	{
		# mouse was clicked outside the listbox... close the listbox
		$w->LbClose;
	}
	else
	{
		# select appropriate entry and close the listbox
		$w->LbCopySelection;
	}
}

# close the listbox after clearing selection
sub LbClose
{
	my ($w) = @_;
	my $l = $w->Subwidget("slistbox")->Subwidget("listbox");
	$l->configure(-selectmode => 'browse');
	$l->selection("clear", 0, "end");
	$w->Popdown;
}

# copy the selection to the entry and close listbox
sub LbCopySelection
{
	my ($w, $justcopy) = @_;
	my $index = $w->LbIndex;
	if (defined $index)
	{
		$w->{"curIndex"} = $index;
		my $l = $w->Subwidget("slistbox")->Subwidget("listbox");
		$l->configure(-selectmode => 'browse');
		my $var_ref = $w->cget( "-textvariable" );
		$$var_ref = $l->get($index);
        #$var_ref = $w->cget("-listok") || undef;;
        #$$var_ref = 1  if (defined($var_ref));
	}
	$w->Popdown  if ($w->{"popped"} && !$justcopy);
	$w->Callback(-browsecmd => $w, $w->Subwidget('entry')->get);
}

sub LbFindSelection
{
	my ($w, $srchval) = @_;

	my $lettersearch = 0;
	if ($srchval)
	{
		$lettersearch = 1;
	}
	else
	{
		my $var_ref = $w->cget( "-textvariable" );
		$srchval = $$var_ref;
	}
	my $l = $w->Subwidget("slistbox")->Subwidget("listbox");
	$l->configure(-selectmode => 'browse');
	my (@listsels) = $l->get('0','end');
	unless ($lettersearch)
	{
		foreach my $i (0..$#listsels)
		{
			if ($listsels[$i] eq $srchval)
			{
				$l->selectionClear('0','end');
				$l->activate($i);
				$l->selectionSet($i);
				$l->update();
				$l->see($i);
				return 1;
			}
		}
	}
	my $index = $w->LbIndex;   #ADDED 20020711 TO ALLOW WRAPPING IF SAME LETTER PRESSED AGAIN!
	foreach my $i (0..$#listsels)
	{
		++$index;
		$index = 0  if ($index > $#listsels);
		if ($listsels[$index] =~ /^$srchval/i)
		{
			$l->selectionClear('0','end');
			$l->activate($index);
			$l->selectionSet($index);
			$l->update();
			$l->see($index);
        			#$var_ref = $w->cget("-listok") || undef;;
        			#$$var_ref = 1  if (defined($var_ref));
			return -1;
		}
	}
     #$var_ref = $w->cget("-listok") || undef;;
     #$$var_ref = 0  if (defined($var_ref));
	return 0;
}

sub LbIndex
{
	my ($w, $flag) = @_;
	my $sel = $w->Subwidget("slistbox")->Subwidget("listbox")->curselection 
	|| $w->Subwidget("slistbox")->Subwidget("listbox")->index('active');

	if (defined $sel)
	{
		return int($sel);
	}
	else
	{
		if (defined $flag && ($flag eq "emptyOK"))
		{
			return undef;
		}
		else
		{
			return 0;
		}
	}
}

# pop down the listbox
sub Popdown
{
	my ($w, $flag) = @_;
	my ($state) = $w->cget( "-state" );

	if ($w->{'savefocus'} && Tk::Exists($w->{'savefocus'}))
	{
		$w->{'savefocus'}->focus;
		delete $w->{'savefocus'};
	}
	if ($w->{"popped"})
	{
		my $c = $w->Subwidget("choices");
		$c->withdraw;
		$w->grabRelease;
		$w->{"popped"} = 0;
		$w->Subwidget("entry")->focus  unless ($flag);
		$w->Subwidget("entry")->selectionRange(0,'end');
	}
}

# This hack is to prevent the ugliness of the arrow being depressed.
#
sub ButtonHack
{
	my ($w) = @_;
	my $b = $w->Subwidget("arrow");

	#JWT: NEXT 6 LINES ADDED TO UNPOP MENU IF BUTTON PRESSED OUTSIDE OF LISTBOX.

	my $s = $w->Subwidget("slistbox");
	my $e = $s->XEvent;
	unless (defined($e))
	{
		$w->LbClose;
	}

	if ($w->{"buttonHack"})
	{
		$b->butUp;
	}
}

sub choices
{
	my $w = shift;
	unless( @_ )
	{
		return( $w->get( qw/0 end/ ) );
	}
	else
	{
		my $choices = shift;
		if( $choices )
		{
			$w->delete( qw/0 end/ );
			$w->Subwidget("slistbox")->insert( "end", @$choices );
		}
		unless ($w->{-listwidth})
		{
			my @l = $w->Subwidget("slistbox")->get(0, 'end');
			my $width = 0;
			for (my $i=0;$i<=$#l;$i++)
			{
				$width = length($l[$i])  if ($width < length($l[$i]));
			}
			$w->Subwidget("entry")->configure(-width => $width);
		}
		return( "" );
	}
}

sub insert
{
	my $w = shift;
	my $pos = shift || 'end';
	my $item = shift;
	if (ref($item))
	{
		$w->Subwidget("slistbox")->insert($pos, @$item);
	}
	else
	{
		$w->Subwidget("slistbox")->insert($pos, ($item, @_));
	}
	unless ($w->{-listwidth})
	{
		my @l = $w->Subwidget("slistbox")->insert(0, 'end');
		my $width = 0;
		for (my $i=0;$i<=$#l;$i++)
		{
			$width = length($l[$i])  if ($width < length($l[$i]));
		}
		$w->Subwidget("entry")->configure(-width => $width);
	}
}

sub curselection
{
	return shift->Subwidget("slistbox")->curselection;
}

sub _set_edit_state
{
	my( $w, $state ) = @_;
	my $entry  = $w->Subwidget( "entry" );
	my $frame  = $w->Subwidget( "frame" );
	my $button = $w->Subwidget( "arrow" );
	my ($color, $txtcolor, $hlcolor);         #6 TO MAKE ENTRY FIELDS LOOK WINDOSEY!

	if ($^O =~ /Win/i)
	{
		if ($state eq 'disabled' || $state eq 'readonly')
		{
			$color = "SystemButtonFace";
		}
		else
		{# Not Editable
			$color = $w->cget( -background );
			$color = 'SystemWindow'  if ($color eq 'SystemButtonFace');
		}
		$entry->configure( -background => $color );
	}
	else
	{
		if ($w->cget( "-colorstate" ))
		{
			if( $state eq "normal" )
			{# Editable
				$color = "gray95";
			}
			else
			{# Not Editable
				$color = $w->cget( -background ) || "lightgray";
			}
			$entry->configure( -background => $color );
		}
	}
	$txtcolor = $w->{-foreground} || $w->cget( -foreground )  unless ($state eq "disabled");

	if( $state eq "readonly" )
	{
		$hlcolor = $w->{-foreground} || $entry->cget( -foreground );
		$entry->configure( -state => "disabled", -takefocus => 0, 
				-foreground => $txtcolor);
		if ($^O =~ /Win/i)
		{
			$button->configure( -state => "normal", -takefocus => 0, -relief => 'raised');
			$frame->configure(-relief => ($w->{-relief} || 'groove'), -takefocus => 1, -highlightcolor => $hlcolor);
		}
		else
		{
			$button->configure( -state => "normal", -takefocus => 0, -relief => 'flat');
			$frame->configure(-relief => ($w->{-relief} || 'raised'), -takefocus => 1, -highlightcolor => $hlcolor);
		}
	}
	elsif ($state eq "textonly" )
	{
		$hlcolor = $w->{-background} || $entry->cget( -background );
		$button->configure( -state => "disabled", -takefocus => 0, -relief => 'raised');
		$frame->configure(-relief => ($w->{-relief} || 'sunken'), -takefocus => 0, -highlightcolor => $hlcolor);
		$entry->configure( -state => 'normal', -takefocus => (1 & ($w->{takefocus} || $w->{btntakesfocus})), 
				-foreground => $txtcolor);
	}
	elsif ($state eq "disabled" )
	{
		$hlcolor = $w->{-background} || $entry->cget( -background );
		$entry->configure( -state => "disabled", -takefocus => 0, 
				-foreground => 'gray50');
		if ($^O =~ /Win/i)
		{
			$button->configure(-state => "disabled",  -takefocus => 0, 
					-relief => 'flat');
		}
		else
		{
			$button->configure(-state => "disabled",  -takefocus => 0, 
					-relief => 'raised');
		}
		$frame->configure(-relief => ($w->{-relief} || 'groove'), -takefocus => 0, -highlightcolor => $hlcolor);
	}
	else
	{
		$hlcolor = $w->{-background} || $entry->cget( -background );
		$entry->configure( -state => $state, -takefocus => (1 & $w->{takefocus}), 
				-foreground => $txtcolor);
		$button->configure( -state => $state, -relief => 'raised', 
				-takefocus => $w->{btntakesfocus});
		$frame->configure(-relief => ($w->{-relief} || 'sunken'), 
				-takefocus => 0, -highlightcolor => $hlcolor);
	}       
}

sub state
{
	my $w = shift;
	unless( @_ )
	{
		return( $w->{Configure}{-state} );
	}
	else
	{
		my $state = shift;
		$w->{Configure}{-state} = $state;
		$w->_set_edit_state( $state );
	}
}

sub _max
{
	my $max = shift;
	foreach my $val (@_)
	{
		$max = $val if $max < $val;
	}
	return( $max );
}

sub shrinkwrap
{
	my( $w, $size ) = @_;

	unless( defined $size )
	{
		$size = _max( map( length, $w->get( qw/0 end/ ) ) ) || 0;;
	}

	my $lb = $w->Subwidget( "slistbox" )->Subwidget( "listbox" );
	$w->configure(  -width => $size );
	$lb->configure( -width => $size );
}

sub setstate
{
	my ($w) = shift;
	my ($state) = shift;

	$w->configure(-state => $state);
	if ($state eq 'textonly')
	{
		$w->Button->configure(-state => 'disabled');
	}
	else
	{
		$w->Button->configure(-state => $state);
	}
}

1;

__END__

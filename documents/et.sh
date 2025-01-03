#!/bin/bash                                                          
                                                                   
# Makes sure emacs daemon is running and opens the file in Emacs in  
# the terminal.                                                      
                                                                   
# If you want to execute elisp, use -e whatever, like so             
                                                                   
# et -e "(message \"Word up\")"                                      
                                                                   
# You may want to redirect that to /dev/null if you don't want the   
# return to printed on the terminal.  Also, just echoing a message   
# may not be visible if Emacs then gives you a message about what    
# to do when do with the frame                                       
                                                                   
# The compliment to this script is ec                                
                                                                   
# Emacsclient option reference                                       
# -a "" starts emacs daemon and reattaches                           
# -t starts in terminal, since I won't be using the gui              
# can also pass in -n if you want to have the shell return right away
                                                                   
exec emacsclient -a "" -t "$@"                                       
divert(-1)dnl -*-m4-*-
changequote(`[[', `]]')

dnl ltrim(str)
define([[ltrim]],[[patsubst([[$1]],[[^[
 	]*]],[[]])]])

dnl rtrim(str)
define([[rtrim]],[[patsubst([[$1]],[[[
 	]*$]],[[]])]])

dnl trim(str)
define([[trim]],[[rtrim(ltrim([[$1]]))]])

dnl strip_space(str)
define([[strip_space]],[[patsubst([[$1]],[[\([
 	][
 	]*\)]],[[ ]])]])

dnl foreach(var,word-list,body)
define([[foreach]],[[dnl
pushdef([[$1]])pushdef([[L$1]])dnl
_foreach([[$1]],trim(translit([[$2]],[[
]],[[   ]])),[[$3]])dnl
popdef([[L$1]])popdef([[$1]])dnl
]])
define([[_foreach]],[[dnl
define([[$1]],regexp([[$2]],[[ *\([^ ]+\)\(.*\)]],[[\1]]))dnl
define([[L$1]],regexp([[$2]],[[ *\([^ ]+\)\(.*\)]],[[\2]]))dnl
ifelse([[$2]],,,[[$3]][[[[]]]][[_foreach([[$1]],L$1,[[$3]])]])dnl
]])

define([[ssh_opt]],[[define([[_opt]],[[$1]])]])

define([[ssh_server]],[[pushdef([[_num]],divnum)divert(1)dnl
Host trim(strip_space([[$1]]))
ifelse(trim([[$2]]),[[]],[[]],[[  Port trim([[$2]])
]])dnl
ifelse(trim([[$3]]),[[]],[[]],[[  User trim([[$3]])
]])dnl
ifelse(trim([[$4]]),[[]],[[]],[[  Hostname trim([[$4]])
]])dnl
ifelse(trim([[$5]]),[[]],[[]],[[  ProxyCommand ssh trim([[$5]]) -W %h:%p
]])dnl
ifelse(trim([[$6]]),[[]],[[]],[[  $6
]])dnl
ifelse(_opt,[[]],[[]],[[  _opt
]])dnl

ifelse(_num,[[]],[[]],divert(_num))popdef([[_num]])]])

set_opt([[]])

divert[[]]dnl

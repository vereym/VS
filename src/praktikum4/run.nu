#!/usr/bin/nu
let hostname = (sys | get host.hostname)

let cookie = 'zummsel'
let bots = ['botA' 'botB' 'botC' 'botD']

def startbots [] {
  for $bot in $bots {
    erl -sname $bot -setcookie $cookie -detached
  }
}

export def-env killbots [] {
  for $bot in $bots {
    let node_name = ([$bot @ $hostname] | str join)
    erl -noshell -eval $"rpc:call\(($node_name), init, stop, []\)." -detached
  }
}

erl -sname towerClock -setcookie $cookie -detached -eval 'towerClock:init().'
erl -sname towerCBC -setcookie $cookie -detached -eval 'towerCBC:init(auto).'
startbots
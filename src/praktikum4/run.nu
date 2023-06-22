#!/usr/bin/nu
let hostname = (sys | get host.hostname)

let cookie = 'zummsel'
let bots = ['botA' 'botB' 'botC' 'botD']

def startbots [] {
  for $bot in $bots {
    erl -sname $bot -setcookie $cookie -detached
  }
}

def killbots [] {
  for $bot in $bots {
    let node_name = ([$bot @ $hostname] | str join)
    erl -noshell -eval $"rpc:call\(($node_name), init, stop, []\)." -detached
  }
}

def setup [] {
  print "starting...\n"
  erl -make | print $in
  erl -sname towerClock -setcookie $cookie -detached -eval 'towerClock:init().'
  erl -sname towerCBC -setcookie $cookie -detached -eval 'towerCBC:init(manu).'
  startbots 
}

def kill [] {
  print "kill...\n"
  killbots
  erl -noshell -eval 'rpc:call(towerCBC, init, stop, []).' -detached
  erl -noshell -eval 'rpc:call(towerClock, init, stop, []).' -detached
}

export def copy_klauck [] {
  let filepath = ("/home/antoni/studium-repos/VS/src/praktikum4/" | path expand --strict)
  print $"filepath: ($filepath)"

  let klauck = [testCBC.beam towerCBC.beam towerClock.beam]
  let config = [towerClock.cfg towerCBC.cfg testCBC.cfg]
  let util = [util.beam vsutil.beam]

  let our = [cbCast.beam]

  let all_files = ($klauck | append $config | append $util | append $our)

  let all_files = ($all_files | each {|file|
    $filepath | path join $file
  })

  $all_files | each {|file|
    cp $file ($filepath | path join von_klauck/)
  }

}

export def main [command: string] {
  match $command {
    "setup" => { setup }
    "kill" => { kill }
    "copy" => { copy_klauck }
  }
}

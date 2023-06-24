#!/usr/bin/nu
let hostname = (sys).host.hostname
let cookie = 'zummsel'
let bots = ['botA' 'botB' 'botC' 'botD']
let tower = $"towerCBC@($hostname)" 
let tower_clock = $"towerClock@($hostname)" 

def setup [] {
  print "starting...\n"
  erl -make | print $in
  erl -sname towerClock -setcookie $cookie -detached -eval 'towerClock:init().'
  # erl -sname towerCBC -setcookie $cookie -detached -eval 'towerCBC:init(manu).'
  for $bot in $bots {
    erl -sname $bot -setcookie $cookie -detached
  }
}

def setup_windows [] {
  print "starting...\n"
  werl -make | print $in
  werl -sname towerClock -setcookie $cookie -detached -eval 'towerClock:init().'
  # erl -sname towerCBC -setcookie $cookie -detached -eval 'towerCBC:init(manu).'
  for $bot in $bots {
    werl -sname $bot -setcookie $cookie -detached
  }
}

def kill [] {
  print "kill...\n"
  for $bot in $bots {
    let node_name = ([$bot @ $hostname] | str join)
    erl -sname $"kill_(random uuid)" -setcookie $cookie -eval $"rpc:call\('($node_name)', init, stop, []\), q()." -detached
  }
  erl -sname $"kill_(random uuid)" -setcookie $cookie -eval $"rpc:call\('($tower)', init, stop, []\), q()." -detached
  erl -sname $"kill_(random uuid)" -setcookie $cookie -eval $"rpc:call\('($tower_clock)', init, stop, []\), q()." -detached
}

def kill_windows [] {
  print "kill...\n"
  for $bot in $bots {
    let node_name = ([$bot @ $hostname] | str join)
    werl -sname $"kill_(random uuid)" -setcookie $cookie -eval $"rpc:call\('($node_name)', init, stop, []\), q()." -detached
  }
  werl -sname $"kill_(random uuid)" -setcookie $cookie -eval $"rpc:call\('($tower)', init, stop, []\), q()." -detached
  werl -sname $"kill_(random uuid)" -setcookie $cookie -eval $"rpc:call\('($tower_clock)', init, stop, []\), q()." -detached
}

def copy_klauck [] {
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

def run [] {
  kill
  sleep 2sec
  setup
}

let help_msg =  {|command| $"unknown command: ($command)\n\navailable commands:\n\tsetup\n\tkill\n\tcopy\n\trun\n"}

export def main [command: string] {
  if $nu.os-info.name == "windows" {
    match $command {
      "setup" => { setup_windows }
      "kill" => { kill_windows }
      "copy" => { copy_klauck }
      _ => { print $help_msg }
    }
  }
  match $command {
    "setup" => { setup }
    "kill" => { kill }
    "copy" => { copy_klauck }
    "run" => { run }
    _ => { print (do $help_msg $command) }
  }
}


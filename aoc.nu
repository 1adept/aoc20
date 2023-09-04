export def main [] {

}

# Load data for a year and day  
export def load [
  year:int             # Year to get
  session_token:string # Session-session_token cookie
  day?:int             # Load a single day
  --location(-l): path # Location to save the data to. Default to './data/'
  -y                   # Assume yes to all
] { 
  mut days = [$day]
  if (null == $day) {
    let current_day = (date now | date to-table | get day | get 0 | into int)
    let remaining_days = (
      0..25
      | each {||} # Hack to convert range to list
      | skip $current_day
    )
    $days = ($remaining_days 
      | prepend "All"
      | input list -m "Choose day(s)"
    )
    if ($days | first | $in == "All") {
      $days = $remaining_days
    } 
  }

  if ( $days | is-empty ) {
    print $"No days given. Bye"
    return
  }

  mut location = $location  
  if (null == $location) {
    $location = (glob -F -S **/data* | input list "Pick location to save data to...")
  }
    
  let base_url = $"https://adventofcode.com/($year)/day"
  for $d in $days {
    let day_filled = if ( $d < 10 ) {
      ($d | fill --alignment right --character '0' --width 2)
    } else {
      $d
    }
    let save_location = $"($location)/($day_filled)_input.in"
    if ( 
      ($save_location | path exists)
      and (not $y)
      and ("No" == (
          ["No" "Yes"] 
          | input list $"File '($save_location)' exists. Overwrite?"i
        ))
    ) {
      # print "Skipping ($day). File exists."
      continue
    }
    

    let url = $"($base_url)/($day)/input"
    let response = http get -H { cookie: $"session=($session_token)" } $url
    ($response | save -f $save_location)
    print $"Wrote ($day) to ($save_location)"
  }
}

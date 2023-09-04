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
  let days = if (null == $day) {
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

  mut location = $location  
  if (null == $location) {
    $location = (glob -F -S **/data* | input list "Pick location to save data to...")
  }
    
  let base_url = $"https://adventofcode.com/($year)/day"
  for $day in $days {
    let day_filled = if ( $day < 10 ) {
      ($day | fill --alignment right --character '0' --width 2)
    } else {
      $day
    }
    let save_location = $"($location)/($day_filled)_input.in"
    if ( 
      ($save_location | path exists)
      and ($y 
        or ("No" == (
          ["No" "Yes"] 
          | input list $"File '($save_location)' exists. Overwrite?")
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

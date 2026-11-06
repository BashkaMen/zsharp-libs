let (<?>) opt def = Option.defaultValue def opt
let (<|>) opt1 opt2 = Option.orElse opt2 opt1

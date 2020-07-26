[<RequireQualifiedAccess>]
module Mode

/// Returns whether the application is in development or production mode
let isDevelopment =
#if DEBUG
    true
#else
    false
#endif

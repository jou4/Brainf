{-# OPTIONS_GHC -cpp #-}

#if defined(mingw32_HOST_OS)
os = "Windows"
#elif defined(linux_HOST_OS)
os = "Linux"
#elif defined(darwin_HOST_OS)
os = "MacOSX"
#else
os = "Other"
#endif

main = putStrLn os

## Install

    $ make
    $ ./bf example/hello.bf

## Usage

execute code

    $ ./bf -e ">++++++++++[<++++++++>-]<."

execute file

    $ ./bf example/hello.bf

output assembly code and execute

    $ ./bf -s example/hello.bf -o output.s
    $ gcc output.s
    $ ./a.out

## License

Copyright (C) 2011 K.Kamitsukasa<br/>
Licensed under the MIT License (http://www.opensource.org/licenses/mit-license.php)

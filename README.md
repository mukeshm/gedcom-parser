# GEDCOM Parser



## Getting Started

### Installation:

To start using GEDCOM parser, install `Haskell Stack tool`

Follow the Operating system specific Instructions at : https://docs.haskellstack.org/en/stable/install_and_upgrade/

### Using GEDCOM parser

Follow the one time setup of stack environment using :

```bash
$ stack setup
```

Stack setup may take some time as it has to download the default haskell compiler and required packages .

Build the project using stack. Execute the below commands in your project directory :

```bash
$ stack build
```

Testing gedcom-parser project using stack:

```bash
$ stack test
```

Executing the gedcom-parser using stack:

```bash
$ stack exec gedcom-parser input_file.txt 
```

#### Example:

Give the filename as the argument to gedcom-parser. It outputs the parsed output to standard out if its successful or prints a error message.

```bash
$ stack exec gedcom-parser gedcom-input.txt	
```

To save the output to file use shel redirect operators to save output to file.

```bash
$ stack exec gedcom-parser gedcom-input.txt > gedcom-output.xml	
```

#### Directories and Files:

- **src** - Contains all modules written as libraries
  - **Parser.hs** - The core parser combinator
  - **Gedcom.hs** - The GEDCOM format specific parsers and xml kind of file generator
- **app** - The main application with main function goes into this directory
  - **Main.hs** - The file containing haskell main function
- **test** - All test related file goes into this directory
  - **Spec.hs** - The main file containing all the tests
- **stack.yaml** - The project stack configuration file
- **gedcom-parser.cabal** - The project cabal configuration file


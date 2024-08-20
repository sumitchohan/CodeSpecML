sumitchohan: @workspace Create a directory structure of all code files in the current codebase and include a brief description of each code file

Workspace: Here is the directory structure of all code files in the current codebase along with a brief description of each code file:

```
Basic Calculator/
	PG16CALC.cbl - COBOL program for a basic calculator.
	README.md - Documentation for the Basic Calculator project.

Basic Roman to Decimal/
	bin/ - Directory for binary files.
	RMNTODEC.cbl - COBOL program to convert Roman numerals to decimal.
	README.md - Documentation for the Basic Roman to Decimal project.

Cohen Sutherland/
	cohenSutherland.cob - COBOL implementation of the Cohen-Sutherland line clipping algorithm.
	readme.md - Documentation for the Cohen Sutherland project.

Contact List Writer/
	bin/ - Directory for binary files.
	PG13ALTCNT.COB - COBOL program for altering contact information.
	PG13CADCTT.COB - COBOL program for adding new contacts.
	PG13CONSCTT.COB - COBOL program for consulting contact information.
	PG13DELCNT.COB - COBOL program for deleting contacts.
	PG13LSCNTT.COB - COBOL program for listing contacts.
	PG13MENU.cbl - COBOL program for the main menu of the contact list application.
	README.md - Documentation for the Contact List Writer project.

CPF - Digits Verification/
	CPF_CALC.COB - COBOL program for CPF digit verification.
	README.md - Documentation for the CPF Digits Verification project.

CSV Writer/
	bin/ - Directory for binary files.
	PRGCSV.cbl - COBOL program for writing data to a CSV file.
	README.md - Documentation for the CSV Writer project.

Dec Is Comma/
	DECISCOMMA.cbl - COBOL program that handles decimal numbers where the decimal separator is a comma.
	LICENSE - License information for the Dec Is Comma project.
	README.md - Documentation for the Dec Is Comma project.

M365 - Admin Center CSV Writer/
	README.md - Documentation for the M365 Admin Center CSV Writer project.

Student Grade Average/
	README.md - Documentation for the Student Grade Average project.

Training Apps/
	PG01HLLWD.cbl - COBOL program that displays "Hello World!".
	README.md - Documentation for the Training Apps project.

README.md - Main documentation file for the entire codebase.
```

Each code file is part of a specific project and serves a unique purpose as described above.

sumitchohan: @workspace create a plant uml diagram showing solution structure

Workspace: Here is a PlantUML diagram representing the structure of your solution:

```plantuml
@startuml
package "Basic Calculator" {
  [PG16CALC.cbl]
  [README.md]
}

package "Basic Roman to Decimal" {
  [bin/]
  [RMNTODEC.cbl]
  [README.md]
}

package "Cohen Sutherland" {
  [cohenSutherland.cob]
  [readme.md]
}

package "Contact List Writer" {
  [bin/]
  [PG13ALTCNT.COB]
  [PG13CADCTT.COB]
  [PG13CONSCTT.COB]
  [PG13DELCNT.COB]
  [PG13LSCNTT.COB]
  [PG13MENU.cbl]
  [README.md]
}

package "CPF - Digits Verification" {
  [CPF_CALC.COB]
  [README.md]
}

package "CSV Writer" {
  [bin/]
  [PRGCSV.cbl]
  [README.md]
}

package "Dec Is Comma" {
  [DECISCOMMA.cbl]
  [LICENSE]
  ...
}

package "M365 - Admin Center CSV Writer" {
  [README.md]
}

package "Student Grade Average" {
}

package "Training Apps" {
  [PG01HLLWD.cbl]
  [PG04ISTDT.cbl]
  [PG06CALC.cbl]
  [PG08MEDIA.cbl]
  [README.md]
}

[README.md]
@enduml
```
![alt text](https://raw.githubusercontent.com/sumitchohan/CodeSpecML/main/cobol-apps-solution-structure.png) 

This diagram shows the hierarchical structure of your solution, with each package representing a directory and each file listed within its respective directory.

sumitchohan: @workspace Provide explanation of above diagram

Workspace: The provided PlantUML diagram represents the structure of your workspace, showing the organization of directories and files. Here's an explanation of each part:

### Packages (Directories)
- **Basic Calculator**: Contains the COBOL program [`PG16CALC.cbl`](command:_github.copilot.openRelativePath?%5B%7B%22scheme%22%3A%22file%22%2C%22authority%22%3A%22%22%2C%22path%22%3A%22%2Fworkspaces%2Fcobol-apps%2FBasic%20Calculator%2FPG16CALC.cbl%22%2C%22query%22%3A%22%22%2C%22fragment%22%3A%22%22%7D%5D "/workspaces/cobol-apps/Basic Calculator/PG16CALC.cbl") and a [`README.md`](command:_github.copilot.openRelativePath?%5B%7B%22scheme%22%3A%22file%22%2C%22authority%22%3A%22%22%2C%22path%22%3A%22%2Fworkspaces%2Fcobol-apps%2FREADME.md%22%2C%22query%22%3A%22%22%2C%22fragment%22%3A%22%22%7D%5D "/workspaces/cobol-apps/README.md") file with documentation.
- **Basic Roman to Decimal**: Contains a `bin/` directory, the COBOL program `RMNTODEC.cbl`, and a [`README.md`](command:_github.copilot.openRelativePath?%5B%7B%22scheme%22%3A%22file%22%2C%22authority%22%3A%22%22%2C%22path%22%3A%22%2Fworkspaces%2Fcobol-apps%2FREADME.md%22%2C%22query%22%3A%22%22%2C%22fragment%22%3A%22%22%7D%5D "/workspaces/cobol-apps/README.md") file.
- **Cohen Sutherland**: Contains the COBOL program [`cohenSutherland.cob`](command:_github.copilot.openRelativePath?%5B%7B%22scheme%22%3A%22file%22%2C%22authority%22%3A%22%22%2C%22path%22%3A%22%2Fworkspaces%2Fcobol-apps%2FCohen%20Sutherland%2FcohenSutherland.cob%22%2C%22query%22%3A%22%22%2C%22fragment%22%3A%22%22%7D%5D "/workspaces/cobol-apps/Cohen Sutherland/cohenSutherland.cob") and a [`readme.md`](command:_github.copilot.openRelativePath?%5B%7B%22scheme%22%3A%22file%22%2C%22authority%22%3A%22%22%2C%22path%22%3A%22%2Fworkspaces%2Fcobol-apps%2FCohen%20Sutherland%2Freadme.md%22%2C%22query%22%3A%22%22%2C%22fragment%22%3A%22%22%7D%5D "/workspaces/cobol-apps/Cohen Sutherland/readme.md") file.
- **Contact List Writer**: Contains a `bin/` directory and several COBOL programs (`PG13ALTCNT.COB`, `PG13CADCTT.COB`, `PG13CONSCTT.COB`, `PG13DELCNT.COB`, `PG13LSCNTT.COB`, `PG13MENU.cbl`) along with a [`README.md`](command:_github.copilot.openRelativePath?%5B%7B%22scheme%22%3A%22file%22%2C%22authority%22%3A%22%22%2C%22path%22%3A%22%2Fworkspaces%2Fcobol-apps%2FREADME.md%22%2C%22query%22%3A%22%22%2C%22fragment%22%3A%22%22%7D%5D "/workspaces/cobol-apps/README.md") file.
- **CPF - Digits Verification**: Contains the COBOL program [`CPF_CALC.COB`](command:_github.copilot.openSymbolFromReferences?%5B%22CPF_CALC.COB%22%2C%5B%7B%22uri%22%3A%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2Fworkspaces%2Fcobol-apps%2FCohen%20Sutherland%2FcohenSutherland.cob%22%2C%22external%22%3A%22file%3A%2F%2F%2Fworkspaces%2Fcobol-apps%2FCohen%2520Sutherland%2FcohenSutherland.cob%22%2C%22path%22%3A%22%2Fworkspaces%2Fcobol-apps%2FCohen%20Sutherland%2FcohenSutherland.cob%22%2C%22scheme%22%3A%22file%22%7D%2C%22pos%22%3A%7B%22line%22%3A176%2C%22character%22%3A16%7D%7D%5D%5D "Go to definition") and a [`README.md`](command:_github.copilot.openRelativePath?%5B%7B%22scheme%22%3A%22file%22%2C%22authority%22%3A%22%22%2C%22path%22%3A%22%2Fworkspaces%2Fcobol-apps%2FREADME.md%22%2C%22query%22%3A%22%22%2C%22fragment%22%3A%22%22%7D%5D "/workspaces/cobol-apps/README.md") file.
- **CSV Writer**: Contains a `bin/` directory, the COBOL program `PRGCSV.cbl`, and a [`README.md`](command:_github.copilot.openRelativePath?%5B%7B%22scheme%22%3A%22file%22%2C%22authority%22%3A%22%22%2C%22path%22%3A%22%2Fworkspaces%2Fcobol-apps%2FREADME.md%22%2C%22query%22%3A%22%22%2C%22fragment%22%3A%22%22%7D%5D "/workspaces/cobol-apps/README.md") file.
- **Dec Is Comma**: Contains the COBOL program `DECISCOMMA.cbl`, a `LICENSE` file, and other files (represented by [`...`](command:_github.copilot.openSymbolFromReferences?%5B%22...%22%2C%5B%7B%22uri%22%3A%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2Fworkspaces%2Fcobol-apps%2FCohen%20Sutherland%2FcohenSutherland.cob%22%2C%22external%22%3A%22file%3A%2F%2F%2Fworkspaces%2Fcobol-apps%2FCohen%2520Sutherland%2FcohenSutherland.cob%22%2C%22path%22%3A%22%2Fworkspaces%2Fcobol-apps%2FCohen%20Sutherland%2FcohenSutherland.cob%22%2C%22scheme%22%3A%22file%22%7D%2C%22pos%22%3A%7B%22line%22%3A127%2C%22character%22%3A15%7D%7D%5D%5D "Go to definition")).
- **M365 - Admin Center CSV Writer**: Contains a [`README.md`](command:_github.copilot.openRelativePath?%5B%7B%22scheme%22%3A%22file%22%2C%22authority%22%3A%22%22%2C%22path%22%3A%22%2Fworkspaces%2Fcobol-apps%2FREADME.md%22%2C%22query%22%3A%22%22%2C%22fragment%22%3A%22%22%7D%5D "/workspaces/cobol-apps/README.md") file.
- **Student Grade Average**: Empty directory.
- **Training Apps**: Contains several COBOL programs ([`PG01HLLWD.cbl`](command:_github.copilot.openRelativePath?%5B%7B%22scheme%22%3A%22file%22%2C%22authority%22%3A%22%22%2C%22path%22%3A%22%2Fworkspaces%2Fcobol-apps%2FTraining%20Apps%2FPG01HLLWD.cbl%22%2C%22query%22%3A%22%22%2C%22fragment%22%3A%22%22%7D%5D "/workspaces/cobol-apps/Training Apps/PG01HLLWD.cbl"), [`PG04ISTDT.cbl`](command:_github.copilot.openRelativePath?%5B%7B%22scheme%22%3A%22file%22%2C%22authority%22%3A%22%22%2C%22path%22%3A%22%2Fworkspaces%2Fcobol-apps%2FTraining%20Apps%2FPG04ISTDT.cbl%22%2C%22query%22%3A%22%22%2C%22fragment%22%3A%22%22%7D%5D "/workspaces/cobol-apps/Training Apps/PG04ISTDT.cbl"), `PG06CALC.cbl`, `PG08MEDIA.cbl`) and a [`README.md`](command:_github.copilot.openRelativePath?%5B%7B%22scheme%22%3A%22file%22%2C%22authority%22%3A%22%22%2C%22path%22%3A%22%2Fworkspaces%2Fcobol-apps%2FREADME.md%22%2C%22query%22%3A%22%22%2C%22fragment%22%3A%22%22%7D%5D "/workspaces/cobol-apps/README.md") file.
- **README.md**: Main documentation file for the entire codebase.

### Files
- **PG16CALC.cbl**: COBOL program for a basic calculator.
- **RMNTODEC.cbl**: COBOL program to convert Roman numerals to decimal.
- **cohenSutherland.cob**: COBOL implementation of the Cohen-Sutherland line clipping algorithm.
- **PG13ALTCNT.COB**: COBOL program for altering contact information.
- **PG13CADCTT.COB**: COBOL program for adding new contacts.
- **PG13CONSCTT.COB**: COBOL program for consulting contact information.
- **PG13DELCNT.COB**: COBOL program for deleting contacts.
- **PG13LSCNTT.COB**: COBOL program for listing contacts.
- **PG13MENU.cbl**: COBOL program for the main menu of the contact list application.
- **CPF_CALC.COB**: COBOL program for CPF digit verification.
- **PRGCSV.cbl**: COBOL program for writing data to a CSV file.
- **DECISCOMMA.cbl**: COBOL program that handles decimal numbers where the decimal separator is a comma.
- **PG01HLLWD.cbl**: COBOL program that displays "Hello World!".
- **PG04ISTDT.cbl**: COBOL program to display user input.
- **PG06CALC.cbl**: COBOL program for a calculator.
- **PG08MEDIA.cbl**: COBOL program for calculating averages.

This structure helps you understand the organization of your workspace and the purpose of each file and directory.

sumitchohan: @workspace generate plant uml component diagrams for all components in current code base

Workspace: Here are the PlantUML component diagrams for all components in your current codebase:

### Basic Calculator
```plantuml
@startuml
package "Basic Calculator" {
  [PG16CALC.cbl] - [README.md]
}
@enduml
```

### Basic Roman to Decimal
```plantuml
@startuml
package "Basic Roman to Decimal" {
  [bin/]
  [RMNTODEC.cbl] - [README.md]
}
@enduml
```

### Cohen Sutherland
```plantuml
@startuml
package "Cohen Sutherland" {
  [cohenSutherland.cob] - [readme.md]
}
@enduml
```

### Contact List Writer
```plantuml
@startuml
package "Contact List Writer" {
  [bin/]
  [PG13ALTCNT.COB] - [README.md]
  [PG13CADCTT.COB] - [README.md]
  [PG13CONSCTT.COB] - [README.md]
  [PG13DELCNT.COB] - [README.md]
  [PG13LSCNTT.COB] - [README.md]
  [PG13MENU.cbl] - [README.md]
}
@enduml
```

### CPF - Digits Verification
```plantuml
@startuml
package "CPF - Digits Verification" {
  [CPF_CALC.COB] - [README.md]
}
@enduml
```

### CSV Writer
```plantuml
@startuml
package "CSV Writer" {
  [bin/]
  [PRGCSV.cbl] - [README.md]
}
@enduml
```

### Dec Is Comma
```plantuml
@startuml
package "Dec Is Comma" {
  [DECISCOMMA.cbl] - [LICENSE]
  [DECISCOMMA.cbl] - [...]
}
@enduml
```

### M365 - Admin Center CSV Writer
```plantuml
@startuml
package "M365 - Admin Center CSV Writer" {
  [README.md]
}
@enduml
```

### Student Grade Average
```plantuml
@startuml
package "Student Grade Average" {
}
@enduml
```

### Training Apps
```plantuml
@startuml
package "Training Apps" {
  [PG01HLLWD.cbl] - [README.md]
  [PG04ISTDT.cbl] - [README.md]
  [PG06CALC.cbl] - [README.md]
  [PG08MEDIA.cbl] - [README.md]
}
@enduml
```

### Main README
```plantuml
@startuml
[README.md]
@enduml
```

These diagrams represent the structure of each component in your codebase, showing the relationships between the files within each directory.

sumitchohan: @workspace Document description of each component in above diagram

Workspace: Here is the description of each component in the provided directory structure:

### Basic Calculator
- **PG16CALC.cbl**: A COBOL program that implements a basic calculator.
- **README.md**: Documentation for the Basic Calculator project, including a link to a YouTube tutorial on creating a calculator with COBOL.

### Basic Roman to Decimal
- **bin/**: Directory for binary files.
- **RMNTODEC.cbl**: A COBOL program that converts Roman numerals to decimal.
- **README.md**: Documentation for the Basic Roman to Decimal project.

### Cohen Sutherland
- **cohenSutherland.cob**: A COBOL implementation of the Cohen-Sutherland line clipping algorithm.
- **readme.md**: Documentation for the Cohen Sutherland project, created for a college activity.

### Contact List Writer
- **bin/**: Directory for binary files.
- **PG13ALTCNT.COB**: A COBOL program for altering contact information.
- **PG13CADCTT.COB**: A COBOL program for adding new contacts.
- **PG13CONSCTT.COB**: A COBOL program for consulting contact information.
- **PG13DELCNT.COB**: A COBOL program for deleting contacts.
- **PG13LSCNTT.COB**: A COBOL program for listing contacts.
- **PG13MENU.cbl**: A COBOL program for the main menu of the contact list application.
- **README.md**: Documentation for the Contact List Writer project, describing the operations for user management.

### CPF - Digits Verification
- **CPF_CALC.COB**: A COBOL program for CPF digit verification.
- **README.md**: Documentation for the CPF Digits Verification project.

### CSV Writer
- **bin/**: Directory for binary files.
- **PRGCSV.cbl**: A COBOL program for writing data to a CSV file.
- **README.md**: Documentation for the CSV Writer project.

### Dec Is Comma
- **DECISCOMMA.cbl**: A COBOL program that handles decimal numbers where the decimal separator is a comma.
- **LICENSE**: License information for the Dec Is Comma project.
- **...**: Placeholder for additional files in the directory.

### M365 - Admin Center CSV Writer
- **README.md**: Documentation for the M365 Admin Center CSV Writer project.

### Student Grade Average
- **Empty Directory**: No files are listed under this directory.

### Training Apps
- **PG01HLLWD.cbl**: A COBOL program that displays "Hello World!".
- **PG04ISTDT.cbl**: A COBOL program to display user input.
- **PG06CALC.cbl**: A COBOL program for a calculator.
- **PG08MEDIA.cbl**: A COBOL program for calculating averages.
- **README.md**: Documentation for the Training Apps project, describing the purpose of creating programs to understand COBOL basics.

### Main README
- **README.md**: Main documentation file for the entire codebase.

These descriptions provide an overview of the purpose and content of each component in your workspace.

sumitchohan: @workspace create another plant uml diagram showing relationships among all business entities and data models

Workspace: To create a PlantUML diagram showing relationships among all business entities and data models in your workspace, we need to identify the key entities and their relationships. Based on the provided information, here is a diagram that represents these relationships:

```plantuml
@startuml
package "Basic Calculator" {
  [PG16CALC.cbl]
}

package "Basic Roman to Decimal" {
  [RMNTODEC.cbl]
}

package "Cohen Sutherland" {
  [cohenSutherland.cob]
}

package "Contact List Writer" {
  [PG13ALTCNT.COB]
  [PG13CADCTT.COB]
  [PG13CONSCTT.COB]
  [PG13DELCNT.COB]
  [PG13LSCNTT.COB]
  [PG13MENU.cbl]
}

package "CPF - Digits Verification" {
  [CPF_CALC.COB]
}

package "CSV Writer" {
  [PRGCSV.cbl]
}

package "Dec Is Comma" {
  [DECISCOMMA.cbl]
}

package "M365 - Admin Center CSV Writer" {
}

package "Student Grade Average" {
}

package "Training Apps" {
  [PG01HLLWD.cbl]
  [PG04ISTDT.cbl]
  [PG06CALC.cbl]
  [PG08MEDIA.cbl]
}

entity "Contact" as Contact {
  +ID: int
  +Name: string
}

entity "CPF" as CPF {
  +Number: string
  +VerificationDigits: string
}

entity "Student" as Student {
  +Name: string
  +Subject: string
  +Grade: float
}

entity "CSVRecord" as CSVRecord {
  +Fields: string[]
}

Contact -- [PG13ALTCNT.COB]
Contact -- [PG13CADCTT.COB]
Contact -- [PG13CONSCTT.COB]
Contact -- [PG13DELCNT.COB]
Contact -- [PG13LSCNTT.COB]
Contact -- [PG13MENU.cbl]

CPF -- [CPF_CALC.COB]

CSVRecord -- [PRGCSV.cbl]

Student -- [PG08MEDIA.cbl]
@enduml
```

### Explanation
- **Entities**:
  - **Contact**: Represents a contact with an ID and Name.
  - **CPF**: Represents a CPF number with verification digits.
  - **Student**: Represents a student with a name, subject, and grade.
  - **CSVRecord**: Represents a record in a CSV file with multiple fields.

### Relationships
- **Contact**:
  - Related to [`PG13ALTCNT.COB`](command:_github.copilot.openRelativePath?%5B%7B%22scheme%22%3A%22file%22%2C%22authority%22%3A%22%22%2C%22path%22%3A%22%2Fworkspaces%2Fcobol-apps%2FContact%20List%20Writer%2FPG13ALTCNT.COB%22%2C%22query%22%3A%22%22%2C%22fragment%22%3A%22%22%7D%5D "/workspaces/cobol-apps/Contact List Writer/PG13ALTCNT.COB"), [`PG13CADCTT.COB`](command:_github.copilot.openRelativePath?%5B%7B%22scheme%22%3A%22file%22%2C%22authority%22%3A%22%22%2C%22path%22%3A%22%2Fworkspaces%2Fcobol-apps%2FContact%20List%20Writer%2FPG13CADCTT.COB%22%2C%22query%22%3A%22%22%2C%22fragment%22%3A%22%22%7D%5D "/workspaces/cobol-apps/Contact List Writer/PG13CADCTT.COB"), [`PG13CONSCTT.COB`](command:_github.copilot.openRelativePath?%5B%7B%22scheme%22%3A%22file%22%2C%22authority%22%3A%22%22%2C%22path%22%3A%22%2Fworkspaces%2Fcobol-apps%2FContact%20List%20Writer%2FPG13CONSCTT.COB%22%2C%22query%22%3A%22%22%2C%22fragment%22%3A%22%22%7D%5D "/workspaces/cobol-apps/Contact List Writer/PG13CONSCTT.COB"), `PG13DELCNT.COB`, `PG13LSCNTT.COB`, and `PG13MENU.cbl` in the **Contact List Writer** package.
- **CPF**:
  - Related to `CPF_CALC.COB` in the **CPF - Digits Verification** package.
- **CSVRecord**:
  - Related to `PRGCSV.cbl` in the **CSV Writer** package.
- **Student**:
  - Related to [`PG08MEDIA.cbl`](command:_github.copilot.openRelativePath?%5B%7B%22scheme%22%3A%22file%22%2C%22authority%22%3A%22%22%2C%22path%22%3A%22%2Fworkspaces%2Fcobol-apps%2FTraining%20Apps%2FPG08MEDIA.cbl%22%2C%22query%22%3A%22%22%2C%22fragment%22%3A%22%22%7D%5D "/workspaces/cobol-apps/Training Apps/PG08MEDIA.cbl") in the **Training Apps** package.

This diagram provides a visual representation of the relationships among business entities and data models in your workspace.

sumitchohan: @workspace Create a plantuml diagram for all classes in current codebase

Workspace: Based on the provided structure and code excerpts, here is a PlantUML diagram representing the classes (or COBOL programs) in your current codebase:

```plantuml
@startuml
package "Basic Calculator" {
  class PG16CALC {
    +main()
  }
}

package "Basic Roman to Decimal" {
  class RMNTODEC {
    +main()
  }
}

package "Cohen Sutherland" {
  class cohenSutherland {
    +main()
    +P-MAIN()
    +P-MAIN-END()
    +P-ENCERRA-FIM()
  }
}

package "Contact List Writer" {
  class PG13ALTCNT {
    +main()
  }
  class PG13CADCTT {
    +main()
  }
  class PG13CONSCTT {
    +main()
  }
  class PG13DELCNT {
    +main()
  }
  class PG13LSCNTT {
    +main()
  }
  class PG13MENU {
    +main()
  }
}

package "CPF - Digits Verification" {
  class CPF_CALC {
    +main()
  }
}

package "CSV Writer" {
  class PRGCSV {
    +main()
  }
}

package "Dec Is Comma" {
  class DECISCOMMA {
    +main()
  }
}

package "M365 - Admin Center CSV Writer" {
  class M365AdminCenterCSVWriter {
    +main()
  }
}

package "Student Grade Average" {
  class StudentGradeAverage {
    +main()
  }
}

package "Training Apps" {
  class PG01HLLWD {
    +main()
  }
  class PG04ISTDT {
    +main()
  }
  class PG06CALC {
    +main()
  }
  class PG08MEDIA {
    +main()
  }
}
@enduml
```

![alt text](https://raw.githubusercontent.com/sumitchohan/CodeSpecML/main/cobol-apps-all-classes.png)

### Explanation
- **Basic Calculator**:
  - **PG16CALC**: Contains a [`main()`](command:_github.copilot.openSymbolFromReferences?%5B%22main()%22%2C%5B%7B%22uri%22%3A%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2Fworkspaces%2Fcobol-apps%2FREADME.md%22%2C%22external%22%3A%22file%3A%2F%2F%2Fworkspaces%2Fcobol-apps%2FREADME.md%22%2C%22path%22%3A%22%2Fworkspaces%2Fcobol-apps%2FREADME.md%22%2C%22scheme%22%3A%22file%22%7D%2C%22pos%22%3A%7B%22line%22%3A4%2C%22character%22%3A76%7D%7D%5D%5D "Go to definition") method.
- **Basic Roman to Decimal**:
  - **RMNTODEC**: Contains a [`main()`](command:_github.copilot.openSymbolFromReferences?%5B%22main()%22%2C%5B%7B%22uri%22%3A%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2Fworkspaces%2Fcobol-apps%2FREADME.md%22%2C%22external%22%3A%22file%3A%2F%2F%2Fworkspaces%2Fcobol-apps%2FREADME.md%22%2C%22path%22%3A%22%2Fworkspaces%2Fcobol-apps%2FREADME.md%22%2C%22scheme%22%3A%22file%22%7D%2C%22pos%22%3A%7B%22line%22%3A4%2C%22character%22%3A76%7D%7D%5D%5D "Go to definition") method.
- **Cohen Sutherland**:
  - **cohenSutherland**: Contains [`main()`](command:_github.copilot.openSymbolFromReferences?%5B%22main()%22%2C%5B%7B%22uri%22%3A%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2Fworkspaces%2Fcobol-apps%2FREADME.md%22%2C%22external%22%3A%22file%3A%2F%2F%2Fworkspaces%2Fcobol-apps%2FREADME.md%22%2C%22path%22%3A%22%2Fworkspaces%2Fcobol-apps%2FREADME.md%22%2C%22scheme%22%3A%22file%22%7D%2C%22pos%22%3A%7B%22line%22%3A4%2C%22character%22%3A76%7D%7D%5D%5D "Go to definition"), [`P-MAIN()`](command:_github.copilot.openSymbolFromReferences?%5B%22P-MAIN()%22%2C%5B%7B%22uri%22%3A%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2Fworkspaces%2Fcobol-apps%2FBasic%20Calculator%2FREADME.md%22%2C%22external%22%3A%22file%3A%2F%2F%2Fworkspaces%2Fcobol-apps%2FBasic%2520Calculator%2FREADME.md%22%2C%22path%22%3A%22%2Fworkspaces%2Fcobol-apps%2FBasic%20Calculator%2FREADME.md%22%2C%22scheme%22%3A%22file%22%7D%2C%22pos%22%3A%7B%22line%22%3A2%2C%22character%22%3A73%7D%7D%2C%7B%22uri%22%3A%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2Fworkspaces%2Fcobol-apps%2FCohen%20Sutherland%2FcohenSutherland.cob%22%2C%22external%22%3A%22file%3A%2F%2F%2Fworkspaces%2Fcobol-apps%2FCohen%2520Sutherland%2FcohenSutherland.cob%22%2C%22path%22%3A%22%2Fworkspaces%2Fcobol-apps%2FCohen%20Sutherland%2FcohenSutherland.cob%22%2C%22scheme%22%3A%22file%22%7D%2C%22pos%22%3A%7B%22line%22%3A145%2C%22character%22%3A6%7D%7D%2C%7B%22uri%22%3A%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2Fworkspaces%2Fcobol-apps%2FDec%20Is%20Comma%2FREADME.md%22%2C%22external%22%3A%22file%3A%2F%2F%2Fworkspaces%2Fcobol-apps%2FDec%2520Is%2520Comma%2FREADME.md%22%2C%22path%22%3A%22%2Fworkspaces%2Fcobol-apps%2FDec%20Is%20Comma%2FREADME.md%22%2C%22scheme%22%3A%22file%22%7D%2C%22pos%22%3A%7B%22line%22%3A0%2C%22character%22%3A18%7D%7D%2C%7B%22uri%22%3A%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2Fworkspaces%2Fcobol-apps%2FREADME.md%22%2C%22external%22%3A%22file%3A%2F%2F%2Fworkspaces%2Fcobol-apps%2FREADME.md%22%2C%22path%22%3A%22%2Fworkspaces%2Fcobol-apps%2FREADME.md%22%2C%22scheme%22%3A%22file%22%7D%2C%22pos%22%3A%7B%22line%22%3A5%2C%22character%22%3A4%7D%7D%2C%7B%22uri%22%3A%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2Fworkspaces%2Fcobol-apps%2FTraining%20Apps%2FPG01HLLWD.cbl%22%2C%22external%22%3A%22file%3A%2F%2F%2Fworkspaces%2Fcobol-apps%2FTraining%2520Apps%2FPG01HLLWD.cbl%22%2C%22path%22%3A%22%2Fworkspaces%2Fcobol-apps%2FTraining%20Apps%2FPG01HLLWD.cbl%22%2C%22scheme%22%3A%22file%22%7D%2C%22pos%22%3A%7B%22line%22%3A3%2C%22character%22%3A8%7D%7D%5D%5D "Go to definition"), [`P-MAIN-END()`](command:_github.copilot.openSymbolFromReferences?%5B%22P-MAIN-END()%22%2C%5B%7B%22uri%22%3A%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2Fworkspaces%2Fcobol-apps%2FBasic%20Calculator%2FREADME.md%22%2C%22external%22%3A%22file%3A%2F%2F%2Fworkspaces%2Fcobol-apps%2FBasic%2520Calculator%2FREADME.md%22%2C%22path%22%3A%22%2Fworkspaces%2Fcobol-apps%2FBasic%20Calculator%2FREADME.md%22%2C%22scheme%22%3A%22file%22%7D%2C%22pos%22%3A%7B%22line%22%3A2%2C%22character%22%3A73%7D%7D%2C%7B%22uri%22%3A%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2Fworkspaces%2Fcobol-apps%2FCohen%20Sutherland%2FcohenSutherland.cob%22%2C%22external%22%3A%22file%3A%2F%2F%2Fworkspaces%2Fcobol-apps%2FCohen%2520Sutherland%2FcohenSutherland.cob%22%2C%22path%22%3A%22%2Fworkspaces%2Fcobol-apps%2FCohen%20Sutherland%2FcohenSutherland.cob%22%2C%22scheme%22%3A%22file%22%7D%2C%22pos%22%3A%7B%22line%22%3A145%2C%22character%22%3A6%7D%7D%2C%7B%22uri%22%3A%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2Fworkspaces%2Fcobol-apps%2FDec%20Is%20Comma%2FREADME.md%22%2C%22external%22%3A%22file%3A%2F%2F%2Fworkspaces%2Fcobol-apps%2FDec%2520Is%2520Comma%2FREADME.md%22%2C%22path%22%3A%22%2Fworkspaces%2Fcobol-apps%2FDec%20Is%20Comma%2FREADME.md%22%2C%22scheme%22%3A%22file%22%7D%2C%22pos%22%3A%7B%22line%22%3A0%2C%22character%22%3A18%7D%7D%2C%7B%22uri%22%3A%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2Fworkspaces%2Fcobol-apps%2FREADME.md%22%2C%22external%22%3A%22file%3A%2F%2F%2Fworkspaces%2Fcobol-apps%2FREADME.md%22%2C%22path%22%3A%22%2Fworkspaces%2Fcobol-apps%2FREADME.md%22%2C%22scheme%22%3A%22file%22%7D%2C%22pos%22%3A%7B%22line%22%3A5%2C%22character%22%3A4%7D%7D%2C%7B%22uri%22%3A%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2Fworkspaces%2Fcobol-apps%2FTraining%20Apps%2FPG01HLLWD.cbl%22%2C%22external%22%3A%22file%3A%2F%2F%2Fworkspaces%2Fcobol-apps%2FTraining%2520Apps%2FPG01HLLWD.cbl%22%2C%22path%22%3A%22%2Fworkspaces%2Fcobol-apps%2FTraining%20Apps%2FPG01HLLWD.cbl%22%2C%22scheme%22%3A%22file%22%7D%2C%22pos%22%3A%7B%22line%22%3A3%2C%22character%22%3A8%7D%7D%5D%5D "Go to definition"), and [`P-ENCERRA-FIM()`](command:_github.copilot.openSymbolFromReferences?%5B%22P-ENCERRA-FIM()%22%2C%5B%7B%22uri%22%3A%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2Fworkspaces%2Fcobol-apps%2FBasic%20Calculator%2FREADME.md%22%2C%22external%22%3A%22file%3A%2F%2F%2Fworkspaces%2Fcobol-apps%2FBasic%2520Calculator%2FREADME.md%22%2C%22path%22%3A%22%2Fworkspaces%2Fcobol-apps%2FBasic%20Calculator%2FREADME.md%22%2C%22scheme%22%3A%22file%22%7D%2C%22pos%22%3A%7B%22line%22%3A2%2C%22character%22%3A73%7D%7D%2C%7B%22uri%22%3A%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2Fworkspaces%2Fcobol-apps%2FCohen%20Sutherland%2FcohenSutherland.cob%22%2C%22external%22%3A%22file%3A%2F%2F%2Fworkspaces%2Fcobol-apps%2FCohen%2520Sutherland%2FcohenSutherland.cob%22%2C%22path%22%3A%22%2Fworkspaces%2Fcobol-apps%2FCohen%20Sutherland%2FcohenSutherland.cob%22%2C%22scheme%22%3A%22file%22%7D%2C%22pos%22%3A%7B%22line%22%3A145%2C%22character%22%3A6%7D%7D%2C%7B%22uri%22%3A%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2Fworkspaces%2Fcobol-apps%2FDec%20Is%20Comma%2FREADME.md%22%2C%22external%22%3A%22file%3A%2F%2F%2Fworkspaces%2Fcobol-apps%2FDec%2520Is%2520Comma%2FREADME.md%22%2C%22path%22%3A%22%2Fworkspaces%2Fcobol-apps%2FDec%20Is%20Comma%2FREADME.md%22%2C%22scheme%22%3A%22file%22%7D%2C%22pos%22%3A%7B%22line%22%3A0%2C%22character%22%3A18%7D%7D%2C%7B%22uri%22%3A%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2Fworkspaces%2Fcobol-apps%2FREADME.md%22%2C%22external%22%3A%22file%3A%2F%2F%2Fworkspaces%2Fcobol-apps%2FREADME.md%22%2C%22path%22%3A%22%2Fworkspaces%2Fcobol-apps%2FREADME.md%22%2C%22scheme%22%3A%22file%22%7D%2C%22pos%22%3A%7B%22line%22%3A5%2C%22character%22%3A4%7D%7D%2C%7B%22uri%22%3A%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2Fworkspaces%2Fcobol-apps%2FTraining%20Apps%2FPG01HLLWD.cbl%22%2C%22external%22%3A%22file%3A%2F%2F%2Fworkspaces%2Fcobol-apps%2FTraining%2520Apps%2FPG01HLLWD.cbl%22%2C%22path%22%3A%22%2Fworkspaces%2Fcobol-apps%2FTraining%20Apps%2FPG01HLLWD.cbl%22%2C%22scheme%22%3A%22file%22%7D%2C%22pos%22%3A%7B%22line%22%3A3%2C%22character%22%3A8%7D%7D%5D%5D "Go to definition") methods.
- **Contact List Writer**:
  - **PG13ALTCNT**: Contains a [`main()`](command:_github.copilot.openSymbolFromReferences?%5B%22main()%22%2C%5B%7B%22uri%22%3A%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2Fworkspaces%2Fcobol-apps%2FREADME.md%22%2C%22external%22%3A%22file%3A%2F%2F%2Fworkspaces%2Fcobol-apps%2FREADME.md%22%2C%22path%22%3A%22%2Fworkspaces%2Fcobol-apps%2FREADME.md%22%2C%22scheme%22%3A%22file%22%7D%2C%22pos%22%3A%7B%22line%22%3A4%2C%22character%22%3A76%7D%7D%5D%5D "Go to definition") method.
  - **PG13CADCTT**: Contains a [`main()`](command:_github.copilot.openSymbolFromReferences?%5B%22main()%22%2C%5B%7B%22uri%22%3A%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2Fworkspaces%2Fcobol-apps%2FREADME.md%22%2C%22external%22%3A%22file%3A%2F%2F%2Fworkspaces%2Fcobol-apps%2FREADME.md%22%2C%22path%22%3A%22%2Fworkspaces%2Fcobol-apps%2FREADME.md%22%2C%22scheme%22%3A%22file%22%7D%2C%22pos%22%3A%7B%22line%22%3A4%2C%22character%22%3A76%7D%7D%5D%5D "Go to definition") method.
  - **PG13CONSCTT**: Contains a [`main()`](command:_github.copilot.openSymbolFromReferences?%5B%22main()%22%2C%5B%7B%22uri%22%3A%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2Fworkspaces%2Fcobol-apps%2FREADME.md%22%2C%22external%22%3A%22file%3A%2F%2F%2Fworkspaces%2Fcobol-apps%2FREADME.md%22%2C%22path%22%3A%22%2Fworkspaces%2Fcobol-apps%2FREADME.md%22%2C%22scheme%22%3A%22file%22%7D%2C%22pos%22%3A%7B%22line%22%3A4%2C%22character%22%3A76%7D%7D%5D%5D "Go to definition") method.
  - **PG13DELCNT**: Contains a [`main()`](command:_github.copilot.openSymbolFromReferences?%5B%22main()%22%2C%5B%7B%22uri%22%3A%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2Fworkspaces%2Fcobol-apps%2FREADME.md%22%2C%22external%22%3A%22file%3A%2F%2F%2Fworkspaces%2Fcobol-apps%2FREADME.md%22%2C%22path%22%3A%22%2Fworkspaces%2Fcobol-apps%2FREADME.md%22%2C%22scheme%22%3A%22file%22%7D%2C%22pos%22%3A%7B%22line%22%3A4%2C%22character%22%3A76%7D%7D%5D%5D "Go to definition") method.
  - **PG13LSCNTT**: Contains a [`main()`](command:_github.copilot.openSymbolFromReferences?%5B%22main()%22%2C%5B%7B%22uri%22%3A%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2Fworkspaces%2Fcobol-apps%2FREADME.md%22%2C%22external%22%3A%22file%3A%2F%2F%2Fworkspaces%2Fcobol-apps%2FREADME.md%22%2C%22path%22%3A%22%2Fworkspaces%2Fcobol-apps%2FREADME.md%22%2C%22scheme%22%3A%22file%22%7D%2C%22pos%22%3A%7B%22line%22%3A4%2C%22character%22%3A76%7D%7D%5D%5D "Go to definition") method.
  - **PG13MENU**: Contains a [`main()`](command:_github.copilot.openSymbolFromReferences?%5B%22main()%22%2C%5B%7B%22uri%22%3A%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2Fworkspaces%2Fcobol-apps%2FREADME.md%22%2C%22external%22%3A%22file%3A%2F%2F%2Fworkspaces%2Fcobol-apps%2FREADME.md%22%2C%22path%22%3A%22%2Fworkspaces%2Fcobol-apps%2FREADME.md%22%2C%22scheme%22%3A%22file%22%7D%2C%22pos%22%3A%7B%22line%22%3A4%2C%22character%22%3A76%7D%7D%5D%5D "Go to definition") method.
- **CPF - Digits Verification**:
  - **CPF_CALC**: Contains a [`main()`](command:_github.copilot.openSymbolFromReferences?%5B%22main()%22%2C%5B%7B%22uri%22%3A%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2Fworkspaces%2Fcobol-apps%2FREADME.md%22%2C%22external%22%3A%22file%3A%2F%2F%2Fworkspaces%2Fcobol-apps%2FREADME.md%22%2C%22path%22%3A%22%2Fworkspaces%2Fcobol-apps%2FREADME.md%22%2C%22scheme%22%3A%22file%22%7D%2C%22pos%22%3A%7B%22line%22%3A4%2C%22character%22%3A76%7D%7D%5D%5D "Go to definition") method.
- **CSV Writer**:
  - **PRGCSV**: Contains a [`main()`](command:_github.copilot.openSymbolFromReferences?%5B%22main()%22%2C%5B%7B%22uri%22%3A%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2Fworkspaces%2Fcobol-apps%2FREADME.md%22%2C%22external%22%3A%22file%3A%2F%2F%2Fworkspaces%2Fcobol-apps%2FREADME.md%22%2C%22path%22%3A%22%2Fworkspaces%2Fcobol-apps%2FREADME.md%22%2C%22scheme%22%3A%22file%22%7D%2C%22pos%22%3A%7B%22line%22%3A4%2C%22character%22%3A76%7D%7D%5D%5D "Go to definition") method.
- **Dec Is Comma**:
  - **DECISCOMMA**: Contains a [`main()`](command:_github.copilot.openSymbolFromReferences?%5B%22main()%22%2C%5B%7B%22uri%22%3A%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2Fworkspaces%2Fcobol-apps%2FREADME.md%22%2C%22external%22%3A%22file%3A%2F%2F%2Fworkspaces%2Fcobol-apps%2FREADME.md%22%2C%22path%22%3A%22%2Fworkspaces%2Fcobol-apps%2FREADME.md%22%2C%22scheme%22%3A%22file%22%7D%2C%22pos%22%3A%7B%22line%22%3A4%2C%22character%22%3A76%7D%7D%5D%5D "Go to definition") method.
- **M365 - Admin Center CSV Writer**:
  - **M365AdminCenterCSVWriter**: Contains a [`main()`](command:_github.copilot.openSymbolFromReferences?%5B%22main()%22%2C%5B%7B%22uri%22%3A%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2Fworkspaces%2Fcobol-apps%2FREADME.md%22%2C%22external%22%3A%22file%3A%2F%2F%2Fworkspaces%2Fcobol-apps%2FREADME.md%22%2C%22path%22%3A%22%2Fworkspaces%2Fcobol-apps%2FREADME.md%22%2C%22scheme%22%3A%22file%22%7D%2C%22pos%22%3A%7B%22line%22%3A4%2C%22character%22%3A76%7D%7D%5D%5D "Go to definition") method.
- **Student Grade Average**:
  - **StudentGradeAverage**: Contains a [`main()`](command:_github.copilot.openSymbolFromReferences?%5B%22main()%22%2C%5B%7B%22uri%22%3A%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2Fworkspaces%2Fcobol-apps%2FREADME.md%22%2C%22external%22%3A%22file%3A%2F%2F%2Fworkspaces%2Fcobol-apps%2FREADME.md%22%2C%22path%22%3A%22%2Fworkspaces%2Fcobol-apps%2FREADME.md%22%2C%22scheme%22%3A%22file%22%7D%2C%22pos%22%3A%7B%22line%22%3A4%2C%22character%22%3A76%7D%7D%5D%5D "Go to definition") method.
- **Training Apps**:
  - **PG01HLLWD**: Contains a [`main()`](command:_github.copilot.openSymbolFromReferences?%5B%22main()%22%2C%5B%7B%22uri%22%3A%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2Fworkspaces%2Fcobol-apps%2FREADME.md%22%2C%22external%22%3A%22file%3A%2F%2F%2Fworkspaces%2Fcobol-apps%2FREADME.md%22%2C%22path%22%3A%22%2Fworkspaces%2Fcobol-apps%2FREADME.md%22%2C%22scheme%22%3A%22file%22%7D%2C%22pos%22%3A%7B%22line%22%3A4%2C%22character%22%3A76%7D%7D%5D%5D "Go to definition") method.
  - **PG04ISTDT**: Contains a [`main()`](command:_github.copilot.openSymbolFromReferences?%5B%22main()%22%2C%5B%7B%22uri%22%3A%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2Fworkspaces%2Fcobol-apps%2FREADME.md%22%2C%22external%22%3A%22file%3A%2F%2F%2Fworkspaces%2Fcobol-apps%2FREADME.md%22%2C%22path%22%3A%22%2Fworkspaces%2Fcobol-apps%2FREADME.md%22%2C%22scheme%22%3A%22file%22%7D%2C%22pos%22%3A%7B%22line%22%3A4%2C%22character%22%3A76%7D%7D%5D%5D "Go to definition") method.
  - **PG06CALC**: Contains a [`main()`](command:_github.copilot.openSymbolFromReferences?%5B%22main()%22%2C%5B%7B%22uri%22%3A%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2Fworkspaces%2Fcobol-apps%2FREADME.md%22%2C%22external%22%3A%22file%3A%2F%2F%2Fworkspaces%2Fcobol-apps%2FREADME.md%22%2C%22path%22%3A%22%2Fworkspaces%2Fcobol-apps%2FREADME.md%22%2C%22scheme%22%3A%22file%22%7D%2C%22pos%22%3A%7B%22line%22%3A4%2C%22character%22%3A76%7D%7D%5D%5D "Go to definition") method.
  - **PG08MEDIA**: Contains a [`main()`](command:_github.copilot.openSymbolFromReferences?%5B%22main()%22%2C%5B%7B%22uri%22%3A%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2Fworkspaces%2Fcobol-apps%2FREADME.md%22%2C%22external%22%3A%22file%3A%2F%2F%2Fworkspaces%2Fcobol-apps%2FREADME.md%22%2C%22path%22%3A%22%2Fworkspaces%2Fcobol-apps%2FREADME.md%22%2C%22scheme%22%3A%22file%22%7D%2C%22pos%22%3A%7B%22line%22%3A4%2C%22character%22%3A76%7D%7D%5D%5D "Go to definition") method.

This diagram provides a visual representation of the classes (COBOL programs) in your codebase and their main methods.

sumitchohan: @workspace generate plant uml diagram showing all use cases in current codebase 

Workspace: To generate a PlantUML diagram showing all use cases in the current codebase, we need to identify the main functionalities of each program. Based on the provided information, here is a PlantUML diagram that represents the use cases:

```plantuml
@startuml

actor User

package "Basic Calculator" {
  usecase "Calculate" as UC_Calculate
}

package "Basic Roman to Decimal" {
  usecase "Convert Roman to Decimal" as UC_RomanToDecimal
}

package "Cohen Sutherland" {
  usecase "Perform Cohen Sutherland Clipping" as UC_CohenSutherland
}

package "Contact List Writer" {
  usecase "Create Contact" as UC_CreateContact
  usecase "Search Contact" as UC_SearchContact
  usecase "List Contacts" as UC_ListContacts
  usecase "Delete Contact" as UC_DeleteContact
  usecase "Change Contact" as UC_ChangeContact
}

package "CPF - Digits Verification" {
  usecase "Verify CPF Digits" as UC_VerifyCPF
}

package "CSV Writer" {
  usecase "Write CSV" as UC_WriteCSV
}

package "Dec Is Comma" {
  usecase "Handle Decimal as Comma" as UC_DecIsComma
}

package "M365 - Admin Center CSV Writer" {
  usecase "Write Admin Center CSV" as UC_WriteAdminCSV
}

package "Training Apps" {
  usecase "Hello World" as UC_HelloWorld
  usecase "Display User Input" as UC_DisplayUserInput
}

package "Student Grade Average" {
  usecase "Calculate Student Grade Average" as UC_StudentGradeAverage
}

User --> UC_Calculate
User --> UC_RomanToDecimal
User --> UC_CohenSutherland
User --> UC_CreateContact
User --> UC_SearchContact
User --> UC_ListContacts
User --> UC_DeleteContact
User --> UC_ChangeContact
User --> UC_VerifyCPF
User --> UC_WriteCSV
User --> UC_DecIsComma
User --> UC_WriteAdminCSV
User --> UC_HelloWorld
User --> UC_DisplayUserInput
User --> UC_StudentGradeAverage

@enduml
```
![alt text](https://raw.githubusercontent.com/sumitchohan/CodeSpecML/main/cobol-apps-all-use-cases.png)

This diagram includes all the use cases inferred from the provided code excerpts and directory structure. You can copy this PlantUML code into a PlantUML viewer to visualize the use case diagram.

sumitchohan:  generate detailed documentation for current code file including all methods, fields, properties and summary of their implementation including pseudo logic

GitHub Copilot: ### Detailed Documentation for [`PG16CALC.cbl`](command:_github.copilot.openRelativePath?%5B%7B%22scheme%22%3A%22file%22%2C%22authority%22%3A%22%22%2C%22path%22%3A%22%2Fworkspaces%2Fcobol-apps%2FBasic%20Calculator%2FPG16CALC.cbl%22%2C%22query%22%3A%22%22%2C%22fragment%22%3A%22%22%7D%5D "/workspaces/cobol-apps/Basic Calculator/PG16CALC.cbl")

#### Summary
This COBOL program is a basic calculator. It is designed to perform arithmetic operations on two numbers. The program is structured into several divisions: Identification, Data, and Procedure.

#### Divisions and Sections

1. **Identification Division**
   - **PROGRAM-ID**: [`PG16CALC`](command:_github.copilot.openSymbolFromReferences?%5B%22PG16CALC%22%2C%5B%7B%22uri%22%3A%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2Fworkspaces%2Fcobol-apps%2FBasic%20Calculator%2FPG16CALC.cbl%22%2C%22external%22%3A%22file%3A%2F%2F%2Fworkspaces%2Fcobol-apps%2FBasic%2520Calculator%2FPG16CALC.cbl%22%2C%22path%22%3A%22%2Fworkspaces%2Fcobol-apps%2FBasic%20Calculator%2FPG16CALC.cbl%22%2C%22scheme%22%3A%22file%22%7D%2C%22pos%22%3A%7B%22line%22%3A6%2C%22character%22%3A19%7D%7D%5D%5D "Go to definition")
     - This is the unique identifier for the program.

2. **Data Division**
   - **File Section**
     - Currently, there are no files defined in this section.
   - **Working-Storage Section**
     - **Fields**
       - [`WS-MODO`](command:_github.copilot.openSymbolFromReferences?%5B%22WS-MODO%22%2C%5B%7B%22uri%22%3A%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2Fworkspaces%2Fcobol-apps%2FBasic%20Calculator%2FPG16CALC.cbl%22%2C%22external%22%3A%22file%3A%2F%2F%2Fworkspaces%2Fcobol-apps%2FBasic%2520Calculator%2FPG16CALC.cbl%22%2C%22path%22%3A%22%2Fworkspaces%2Fcobol-apps%2FBasic%20Calculator%2FPG16CALC.cbl%22%2C%22scheme%22%3A%22file%22%7D%2C%22pos%22%3A%7B%22line%22%3A10%2C%22character%22%3A10%7D%7D%5D%5D "Go to definition") (PIC X(01))
         - This field is used to store the mode of operation (e.g., addition, subtraction).
       - [`WS-NUM-1`](command:_github.copilot.openSymbolFromReferences?%5B%22WS-NUM-1%22%2C%5B%7B%22uri%22%3A%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2Fworkspaces%2Fcobol-apps%2FBasic%20Calculator%2FPG16CALC.cbl%22%2C%22external%22%3A%22file%3A%2F%2F%2Fworkspaces%2Fcobol-apps%2FBasic%2520Calculator%2FPG16CALC.cbl%22%2C%22path%22%3A%22%2Fworkspaces%2Fcobol-apps%2FBasic%20Calculator%2FPG16CALC.cbl%22%2C%22scheme%22%3A%22file%22%7D%2C%22pos%22%3A%7B%22line%22%3A10%2C%22character%22%3A10%7D%7D%5D%5D "Go to definition") (PIC 9(02))
         - This field stores the first number for the calculation.
       - [`WS-NUM-2`](command:_github.copilot.openSymbolFromReferences?%5B%22WS-NUM-2%22%2C%5B%7B%22uri%22%3A%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2Fworkspaces%2Fcobol-apps%2FBasic%20Calculator%2FPG16CALC.cbl%22%2C%22external%22%3A%22file%3A%2F%2F%2Fworkspaces%2Fcobol-apps%2FBasic%2520Calculator%2FPG16CALC.cbl%22%2C%22path%22%3A%22%2Fworkspaces%2Fcobol-apps%2FBasic%20Calculator%2FPG16CALC.cbl%22%2C%22scheme%22%3A%22file%22%7D%2C%22pos%22%3A%7B%22line%22%3A10%2C%22character%22%3A10%7D%7D%5D%5D "Go to definition") (PIC 9(02))
         - This field stores the second number for the calculation.
       - [`WS-RESULT`](command:_github.copilot.openSymbolFromReferences?%5B%22WS-RESULT%22%2C%5B%7B%22uri%22%3A%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2Fworkspaces%2Fcobol-apps%2FBasic%20Calculator%2FPG16CALC.cbl%22%2C%22external%22%3A%22file%3A%2F%2F%2Fworkspaces%2Fcobol-apps%2FBasic%2520Calculator%2FPG16CALC.cbl%22%2C%22path%22%3A%22%2Fworkspaces%2Fcobol-apps%2FBasic%20Calculator%2FPG16CALC.cbl%22%2C%22scheme%22%3A%22file%22%7D%2C%22pos%22%3A%7B%22line%22%3A10%2C%22character%22%3A10%7D%7D%5D%5D "Go to definition") (PIC 9(03))
         - This field stores the result of the calculation.

3. **Procedure Division**
   - This section will contain the logic for the calculator operations. The detailed implementation is not provided in the excerpt, but we can infer the following pseudo logic based on the fields defined:

#### Pseudo Logic

1. **Initialize Program**
   - Display a welcome message.
   - Prompt the user to enter the mode of operation (e.g., addition, subtraction).

2. **Input Numbers**
   - Prompt the user to enter the first number ([`WS-NUM-1`](command:_github.copilot.openSymbolFromReferences?%5B%22WS-NUM-1%22%2C%5B%7B%22uri%22%3A%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2Fworkspaces%2Fcobol-apps%2FBasic%20Calculator%2FPG16CALC.cbl%22%2C%22external%22%3A%22file%3A%2F%2F%2Fworkspaces%2Fcobol-apps%2FBasic%2520Calculator%2FPG16CALC.cbl%22%2C%22path%22%3A%22%2Fworkspaces%2Fcobol-apps%2FBasic%20Calculator%2FPG16CALC.cbl%22%2C%22scheme%22%3A%22file%22%7D%2C%22pos%22%3A%7B%22line%22%3A10%2C%22character%22%3A10%7D%7D%5D%5D "Go to definition")).
   - Prompt the user to enter the second number ([`WS-NUM-2`](command:_github.copilot.openSymbolFromReferences?%5B%22WS-NUM-2%22%2C%5B%7B%22uri%22%3A%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2Fworkspaces%2Fcobol-apps%2FBasic%20Calculator%2FPG16CALC.cbl%22%2C%22external%22%3A%22file%3A%2F%2F%2Fworkspaces%2Fcobol-apps%2FBasic%2520Calculator%2FPG16CALC.cbl%22%2C%22path%22%3A%22%2Fworkspaces%2Fcobol-apps%2FBasic%20Calculator%2FPG16CALC.cbl%22%2C%22scheme%22%3A%22file%22%7D%2C%22pos%22%3A%7B%22line%22%3A10%2C%22character%22%3A10%7D%7D%5D%5D "Go to definition")).

3. **Perform Calculation**
   - Based on the mode ([`WS-MODO`](command:_github.copilot.openSymbolFromReferences?%5B%22WS-MODO%22%2C%5B%7B%22uri%22%3A%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2Fworkspaces%2Fcobol-apps%2FBasic%20Calculator%2FPG16CALC.cbl%22%2C%22external%22%3A%22file%3A%2F%2F%2Fworkspaces%2Fcobol-apps%2FBasic%2520Calculator%2FPG16CALC.cbl%22%2C%22path%22%3A%22%2Fworkspaces%2Fcobol-apps%2FBasic%20Calculator%2FPG16CALC.cbl%22%2C%22scheme%22%3A%22file%22%7D%2C%22pos%22%3A%7B%22line%22%3A10%2C%22character%22%3A10%7D%7D%5D%5D "Go to definition")), perform the corresponding arithmetic operation:
     - If [`WS-MODO`](command:_github.copilot.openSymbolFromReferences?%5B%22WS-MODO%22%2C%5B%7B%22uri%22%3A%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2Fworkspaces%2Fcobol-apps%2FBasic%20Calculator%2FPG16CALC.cbl%22%2C%22external%22%3A%22file%3A%2F%2F%2Fworkspaces%2Fcobol-apps%2FBasic%2520Calculator%2FPG16CALC.cbl%22%2C%22path%22%3A%22%2Fworkspaces%2Fcobol-apps%2FBasic%20Calculator%2FPG16CALC.cbl%22%2C%22scheme%22%3A%22file%22%7D%2C%22pos%22%3A%7B%22line%22%3A10%2C%22character%22%3A10%7D%7D%5D%5D "Go to definition") is addition, add [`WS-NUM-1`](command:_github.copilot.openSymbolFromReferences?%5B%22WS-NUM-1%22%2C%5B%7B%22uri%22%3A%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2Fworkspaces%2Fcobol-apps%2FBasic%20Calculator%2FPG16CALC.cbl%22%2C%22external%22%3A%22file%3A%2F%2F%2Fworkspaces%2Fcobol-apps%2FBasic%2520Calculator%2FPG16CALC.cbl%22%2C%22path%22%3A%22%2Fworkspaces%2Fcobol-apps%2FBasic%20Calculator%2FPG16CALC.cbl%22%2C%22scheme%22%3A%22file%22%7D%2C%22pos%22%3A%7B%22line%22%3A10%2C%22character%22%3A10%7D%7D%5D%5D "Go to definition") and [`WS-NUM-2`](command:_github.copilot.openSymbolFromReferences?%5B%22WS-NUM-2%22%2C%5B%7B%22uri%22%3A%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2Fworkspaces%2Fcobol-apps%2FBasic%20Calculator%2FPG16CALC.cbl%22%2C%22external%22%3A%22file%3A%2F%2F%2Fworkspaces%2Fcobol-apps%2FBasic%2520Calculator%2FPG16CALC.cbl%22%2C%22path%22%3A%22%2Fworkspaces%2Fcobol-apps%2FBasic%20Calculator%2FPG16CALC.cbl%22%2C%22scheme%22%3A%22file%22%7D%2C%22pos%22%3A%7B%22line%22%3A10%2C%22character%22%3A10%7D%7D%5D%5D "Go to definition"), and store the result in [`WS-RESULT`](command:_github.copilot.openSymbolFromReferences?%5B%22WS-RESULT%22%2C%5B%7B%22uri%22%3A%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2Fworkspaces%2Fcobol-apps%2FBasic%20Calculator%2FPG16CALC.cbl%22%2C%22external%22%3A%22file%3A%2F%2F%2Fworkspaces%2Fcobol-apps%2FBasic%2520Calculator%2FPG16CALC.cbl%22%2C%22path%22%3A%22%2Fworkspaces%2Fcobol-apps%2FBasic%20Calculator%2FPG16CALC.cbl%22%2C%22scheme%22%3A%22file%22%7D%2C%22pos%22%3A%7B%22line%22%3A10%2C%22character%22%3A10%7D%7D%5D%5D "Go to definition").
     - If [`WS-MODO`](command:_github.copilot.openSymbolFromReferences?%5B%22WS-MODO%22%2C%5B%7B%22uri%22%3A%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2Fworkspaces%2Fcobol-apps%2FBasic%20Calculator%2FPG16CALC.cbl%22%2C%22external%22%3A%22file%3A%2F%2F%2Fworkspaces%2Fcobol-apps%2FBasic%2520Calculator%2FPG16CALC.cbl%22%2C%22path%22%3A%22%2Fworkspaces%2Fcobol-apps%2FBasic%20Calculator%2FPG16CALC.cbl%22%2C%22scheme%22%3A%22file%22%7D%2C%22pos%22%3A%7B%22line%22%3A10%2C%22character%22%3A10%7D%7D%5D%5D "Go to definition") is subtraction, subtract [`WS-NUM-2`](command:_github.copilot.openSymbolFromReferences?%5B%22WS-NUM-2%22%2C%5B%7B%22uri%22%3A%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2Fworkspaces%2Fcobol-apps%2FBasic%20Calculator%2FPG16CALC.cbl%22%2C%22external%22%3A%22file%3A%2F%2F%2Fworkspaces%2Fcobol-apps%2FBasic%2520Calculator%2FPG16CALC.cbl%22%2C%22path%22%3A%22%2Fworkspaces%2Fcobol-apps%2FBasic%20Calculator%2FPG16CALC.cbl%22%2C%22scheme%22%3A%22file%22%7D%2C%22pos%22%3A%7B%22line%22%3A10%2C%22character%22%3A10%7D%7D%5D%5D "Go to definition") from [`WS-NUM-1`](command:_github.copilot.openSymbolFromReferences?%5B%22WS-NUM-1%22%2C%5B%7B%22uri%22%3A%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2Fworkspaces%2Fcobol-apps%2FBasic%20Calculator%2FPG16CALC.cbl%22%2C%22external%22%3A%22file%3A%2F%2F%2Fworkspaces%2Fcobol-apps%2FBasic%2520Calculator%2FPG16CALC.cbl%22%2C%22path%22%3A%22%2Fworkspaces%2Fcobol-apps%2FBasic%20Calculator%2FPG16CALC.cbl%22%2C%22scheme%22%3A%22file%22%7D%2C%22pos%22%3A%7B%22line%22%3A10%2C%22character%22%3A10%7D%7D%5D%5D "Go to definition"), and store the result in [`WS-RESULT`](command:_github.copilot.openSymbolFromReferences?%5B%22WS-RESULT%22%2C%5B%7B%22uri%22%3A%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2Fworkspaces%2Fcobol-apps%2FBasic%20Calculator%2FPG16CALC.cbl%22%2C%22external%22%3A%22file%3A%2F%2F%2Fworkspaces%2Fcobol-apps%2FBasic%2520Calculator%2FPG16CALC.cbl%22%2C%22path%22%3A%22%2Fworkspaces%2Fcobol-apps%2FBasic%20Calculator%2FPG16CALC.cbl%22%2C%22scheme%22%3A%22file%22%7D%2C%22pos%22%3A%7B%22line%22%3A10%2C%22character%22%3A10%7D%7D%5D%5D "Go to definition").
     - (Other operations like multiplication and division can be added similarly.)

4. **Display Result**
   - Display the result ([`WS-RESULT`](command:_github.copilot.openSymbolFromReferences?%5B%22WS-RESULT%22%2C%5B%7B%22uri%22%3A%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2Fworkspaces%2Fcobol-apps%2FBasic%20Calculator%2FPG16CALC.cbl%22%2C%22external%22%3A%22file%3A%2F%2F%2Fworkspaces%2Fcobol-apps%2FBasic%2520Calculator%2FPG16CALC.cbl%22%2C%22path%22%3A%22%2Fworkspaces%2Fcobol-apps%2FBasic%20Calculator%2FPG16CALC.cbl%22%2C%22scheme%22%3A%22file%22%7D%2C%22pos%22%3A%7B%22line%22%3A10%2C%22character%22%3A10%7D%7D%5D%5D "Go to definition")) to the user.

5. **End Program**
   - Display a message indicating the end of the program.
   - Terminate the program.

#### Example Implementation (Pseudo Code)

```plaintext
START PROGRAM
  DISPLAY "Welcome to COBOL Calculator"
  DISPLAY "Enter mode (A for addition, S for subtraction):"
  ACCEPT WS-MODO

  DISPLAY "Enter first number:"
  ACCEPT WS-NUM-1

  DISPLAY "Enter second number:"
  ACCEPT WS-NUM-2

  IF WS-MODO = 'A'
    COMPUTE WS-RESULT = WS-NUM-1 + WS-NUM-2
  ELSE IF WS-MODO = 'S'
    COMPUTE WS-RESULT = WS-NUM-1 - WS-NUM-2
  END-IF

  DISPLAY "The result is: " WS-RESULT

  DISPLAY "End of Program"
END PROGRAM
```

This documentation provides an overview of the program structure, the purpose of each field, and a high-level pseudo logic of how the calculator operations might be implemented.

sumitchohan:  generate detailed documentation for current code file including all methods, fields, properties and summary of their implementation including pseudo logic

GitHub Copilot: ### Detailed Documentation for [`RMNTODEC.cbl`](command:_github.copilot.openRelativePath?%5B%7B%22scheme%22%3A%22file%22%2C%22authority%22%3A%22%22%2C%22path%22%3A%22%2Fworkspaces%2Fcobol-apps%2FBasic%20Roman%20to%20Decimal%2FRMNTODEC.cbl%22%2C%22query%22%3A%22%22%2C%22fragment%22%3A%22%22%7D%5D "/workspaces/cobol-apps/Basic Roman to Decimal/RMNTODEC.cbl")

#### Summary
This COBOL program converts Roman numerals to their decimal (numeric) equivalents. The program is structured into several divisions: Identification, Data, and Procedure.

#### Divisions and Sections

1. **Identification Division**
   - **PROGRAM-ID**: [`RMNTODEC`](command:_github.copilot.openSymbolFromReferences?%5B%22RMNTODEC%22%2C%5B%7B%22uri%22%3A%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2Fworkspaces%2Fcobol-apps%2FBasic%20Roman%20to%20Decimal%2FRMNTODEC.cbl%22%2C%22external%22%3A%22file%3A%2F%2F%2Fworkspaces%2Fcobol-apps%2FBasic%2520Roman%2520to%2520Decimal%2FRMNTODEC.cbl%22%2C%22path%22%3A%22%2Fworkspaces%2Fcobol-apps%2FBasic%20Roman%20to%20Decimal%2FRMNTODEC.cbl%22%2C%22scheme%22%3A%22file%22%7D%2C%22pos%22%3A%7B%22line%22%3A1%2C%22character%22%3A23%7D%7D%5D%5D "Go to definition")
     - This is the unique identifier for the program.

2. **Data Division**
   - **File Section**
     - Currently, there are no files defined in this section.
   - **Working-Storage Section**
     - **Fields**
       - [`WS-ROMAN`](command:_github.copilot.openSymbolFromReferences?%5B%22WS-ROMAN%22%2C%5B%7B%22uri%22%3A%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2Fworkspaces%2Fcobol-apps%2FBasic%20Roman%20to%20Decimal%2FRMNTODEC.cbl%22%2C%22external%22%3A%22file%3A%2F%2F%2Fworkspaces%2Fcobol-apps%2FBasic%2520Roman%2520to%2520Decimal%2FRMNTODEC.cbl%22%2C%22path%22%3A%22%2Fworkspaces%2Fcobol-apps%2FBasic%20Roman%20to%20Decimal%2FRMNTODEC.cbl%22%2C%22scheme%22%3A%22file%22%7D%2C%22pos%22%3A%7B%22line%22%3A14%2C%22character%22%3A10%7D%7D%5D%5D "Go to definition") (PIC X(09) VALUE SPACES)
         - This field is used to store the Roman numeral input by the user.
       - [`WS-NUMERIC`](command:_github.copilot.openSymbolFromReferences?%5B%22WS-NUMERIC%22%2C%5B%7B%22uri%22%3A%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2Fworkspaces%2Fcobol-apps%2FBasic%20Roman%20to%20Decimal%2FRMNTODEC.cbl%22%2C%22external%22%3A%22file%3A%2F%2F%2Fworkspaces%2Fcobol-apps%2FBasic%2520Roman%2520to%2520Decimal%2FRMNTODEC.cbl%22%2C%22path%22%3A%22%2Fworkspaces%2Fcobol-apps%2FBasic%20Roman%20to%20Decimal%2FRMNTODEC.cbl%22%2C%22scheme%22%3A%22file%22%7D%2C%22pos%22%3A%7B%22line%22%3A14%2C%22character%22%3A10%7D%7D%5D%5D "Go to definition") (PIC 9(04) VALUE ZEROES)
         - This field stores the numeric equivalent of the Roman numeral.
       - [`WS-QTDDGT`](command:_github.copilot.openSymbolFromReferences?%5B%22WS-QTDDGT%22%2C%5B%7B%22uri%22%3A%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2Fworkspaces%2Fcobol-apps%2FBasic%20Roman%20to%20Decimal%2FRMNTODEC.cbl%22%2C%22external%22%3A%22file%3A%2F%2F%2Fworkspaces%2Fcobol-apps%2FBasic%2520Roman%2520to%2520Decimal%2FRMNTODEC.cbl%22%2C%22path%22%3A%22%2Fworkspaces%2Fcobol-apps%2FBasic%20Roman%20to%20Decimal%2FRMNTODEC.cbl%22%2C%22scheme%22%3A%22file%22%7D%2C%22pos%22%3A%7B%22line%22%3A14%2C%22character%22%3A10%7D%7D%5D%5D "Go to definition") (PIC 9(02))
         - This field is used to store the quantity of digits in the Roman numeral.

3. **Procedure Division**
   - This section will contain the logic for converting Roman numerals to decimal numbers. The detailed implementation is not provided in the excerpt, but we can infer the following pseudo logic based on the fields defined:

#### Pseudo Logic

1. **Initialize Program**
   - Display a welcome message.
   - Prompt the user to enter a Roman numeral.

2. **Input Roman Numeral**
   - Accept the Roman numeral input from the user and store it in [`WS-ROMAN`](command:_github.copilot.openSymbolFromReferences?%5B%22WS-ROMAN%22%2C%5B%7B%22uri%22%3A%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2Fworkspaces%2Fcobol-apps%2FBasic%20Roman%20to%20Decimal%2FRMNTODEC.cbl%22%2C%22external%22%3A%22file%3A%2F%2F%2Fworkspaces%2Fcobol-apps%2FBasic%2520Roman%2520to%2520Decimal%2FRMNTODEC.cbl%22%2C%22path%22%3A%22%2Fworkspaces%2Fcobol-apps%2FBasic%20Roman%20to%20Decimal%2FRMNTODEC.cbl%22%2C%22scheme%22%3A%22file%22%7D%2C%22pos%22%3A%7B%22line%22%3A14%2C%22character%22%3A10%7D%7D%5D%5D "Go to definition").

3. **Convert Roman Numeral to Decimal**
   - Initialize [`WS-NUMERIC`](command:_github.copilot.openSymbolFromReferences?%5B%22WS-NUMERIC%22%2C%5B%7B%22uri%22%3A%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2Fworkspaces%2Fcobol-apps%2FBasic%20Roman%20to%20Decimal%2FRMNTODEC.cbl%22%2C%22external%22%3A%22file%3A%2F%2F%2Fworkspaces%2Fcobol-apps%2FBasic%2520Roman%2520to%2520Decimal%2FRMNTODEC.cbl%22%2C%22path%22%3A%22%2Fworkspaces%2Fcobol-apps%2FBasic%20Roman%20to%20Decimal%2FRMNTODEC.cbl%22%2C%22scheme%22%3A%22file%22%7D%2C%22pos%22%3A%7B%22line%22%3A14%2C%22character%22%3A10%7D%7D%5D%5D "Go to definition") to zero.
   - Loop through each character in [`WS-ROMAN`](command:_github.copilot.openSymbolFromReferences?%5B%22WS-ROMAN%22%2C%5B%7B%22uri%22%3A%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2Fworkspaces%2Fcobol-apps%2FBasic%20Roman%20to%20Decimal%2FRMNTODEC.cbl%22%2C%22external%22%3A%22file%3A%2F%2F%2Fworkspaces%2Fcobol-apps%2FBasic%2520Roman%2520to%2520Decimal%2FRMNTODEC.cbl%22%2C%22path%22%3A%22%2Fworkspaces%2Fcobol-apps%2FBasic%20Roman%20to%20Decimal%2FRMNTODEC.cbl%22%2C%22scheme%22%3A%22file%22%7D%2C%22pos%22%3A%7B%22line%22%3A14%2C%22character%22%3A10%7D%7D%5D%5D "Go to definition"):
     - Convert each Roman numeral character to its corresponding decimal value.
     - Add or subtract the value to/from [`WS-NUMERIC`](command:_github.copilot.openSymbolFromReferences?%5B%22WS-NUMERIC%22%2C%5B%7B%22uri%22%3A%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2Fworkspaces%2Fcobol-apps%2FBasic%20Roman%20to%20Decimal%2FRMNTODEC.cbl%22%2C%22external%22%3A%22file%3A%2F%2F%2Fworkspaces%2Fcobol-apps%2FBasic%2520Roman%2520to%2520Decimal%2FRMNTODEC.cbl%22%2C%22path%22%3A%22%2Fworkspaces%2Fcobol-apps%2FBasic%20Roman%20to%20Decimal%2FRMNTODEC.cbl%22%2C%22scheme%22%3A%22file%22%7D%2C%22pos%22%3A%7B%22line%22%3A14%2C%22character%22%3A10%7D%7D%5D%5D "Go to definition") based on Roman numeral rules (e.g., if a smaller numeral precedes a larger numeral, it is subtracted).

4. **Store Result**
   - Store the final numeric value in [`WS-NUMERIC`](command:_github.copilot.openSymbolFromReferences?%5B%22WS-NUMERIC%22%2C%5B%7B%22uri%22%3A%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2Fworkspaces%2Fcobol-apps%2FBasic%20Roman%20to%20Decimal%2FRMNTODEC.cbl%22%2C%22external%22%3A%22file%3A%2F%2F%2Fworkspaces%2Fcobol-apps%2FBasic%2520Roman%2520to%2520Decimal%2FRMNTODEC.cbl%22%2C%22path%22%3A%22%2Fworkspaces%2Fcobol-apps%2FBasic%20Roman%20to%20Decimal%2FRMNTODEC.cbl%22%2C%22scheme%22%3A%22file%22%7D%2C%22pos%22%3A%7B%22line%22%3A14%2C%22character%22%3A10%7D%7D%5D%5D "Go to definition").

5. **Display Result**
   - Display the numeric equivalent of the Roman numeral to the user.

6. **End Program**
   - Display a message indicating the end of the program.
   - Terminate the program.

#### Example Implementation (Pseudo Code)

```plaintext
START PROGRAM
  DISPLAY "Welcome to Roman to Decimal Converter"
  DISPLAY "Enter a Roman numeral:"
  ACCEPT WS-ROMAN

  MOVE 0 TO WS-NUMERIC

  PERFORM VARYING INDEX FROM 1 BY 1 UNTIL INDEX > LENGTH OF WS-ROMAN
    EVALUATE WS-ROMAN(INDEX:1)
      WHEN 'I' ADD 1 TO WS-NUMERIC
      WHEN 'V' ADD 5 TO WS-NUMERIC
      WHEN 'X' ADD 10 TO WS-NUMERIC
      WHEN 'L' ADD 50 TO WS-NUMERIC
      WHEN 'C' ADD 100 TO WS-NUMERIC
      WHEN 'D' ADD 500 TO WS-NUMERIC
      WHEN 'M' ADD 1000 TO WS-NUMERIC
    END-EVALUATE
  END-PERFORM

  DISPLAY "The decimal value is: " WS-NUMERIC

  DISPLAY "End of Program"
END PROGRAM
```

This documentation provides an overview of the program structure, the purpose of each field, and a high-level pseudo logic of how the Roman numeral to decimal conversion might be implemented.

sumitchohan:  generate detailed documentation for current code file including all methods, fields, properties and summary of their implementation including pseudo logic

GitHub Copilot: ### Detailed Documentation for [`cohenSutherland.cob`](command:_github.copilot.openRelativePath?%5B%7B%22scheme%22%3A%22file%22%2C%22authority%22%3A%22%22%2C%22path%22%3A%22%2Fworkspaces%2Fcobol-apps%2FCohen%20Sutherland%2FcohenSutherland.cob%22%2C%22query%22%3A%22%22%2C%22fragment%22%3A%22%22%7D%5D "/workspaces/cobol-apps/Cohen Sutherland/cohenSutherland.cob")

#### Summary
This COBOL program implements the Cohen-Sutherland line clipping algorithm. The program is structured into several divisions: Identification, Environment, Data, and Procedure.

#### Divisions and Sections

1. **Identification Division**
   - **PROGRAM-ID**: [`cohenSutherland`](command:_github.copilot.openSymbolFromReferences?%5B%22cohenSutherland%22%2C%5B%7B%22uri%22%3A%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2Fworkspaces%2Fcobol-apps%2FCohen%20Sutherland%2FcohenSutherland.cob%22%2C%22external%22%3A%22file%3A%2F%2F%2Fworkspaces%2Fcobol-apps%2FCohen%2520Sutherland%2FcohenSutherland.cob%22%2C%22path%22%3A%22%2Fworkspaces%2Fcobol-apps%2FCohen%20Sutherland%2FcohenSutherland.cob%22%2C%22scheme%22%3A%22file%22%7D%2C%22pos%22%3A%7B%22line%22%3A7%2C%22character%22%3A19%7D%7D%5D%5D "Go to definition")
     - This is the unique identifier for the program.

2. **Environment Division**
   - **Configuration Section**
     - **SPECIAL-NAMES**
       - [`DECIMAL-POINT IS COMMA`](command:_github.copilot.openSymbolFromReferences?%5B%22DECIMAL-POINT%20IS%20COMMA%22%2C%5B%7B%22uri%22%3A%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2Fworkspaces%2Fcobol-apps%2FCohen%20Sutherland%2FcohenSutherland.cob%22%2C%22external%22%3A%22file%3A%2F%2F%2Fworkspaces%2Fcobol-apps%2FCohen%2520Sutherland%2FcohenSutherland.cob%22%2C%22path%22%3A%22%2Fworkspaces%2Fcobol-apps%2FCohen%20Sutherland%2FcohenSutherland.cob%22%2C%22scheme%22%3A%22file%22%7D%2C%22pos%22%3A%7B%22line%22%3A12%2C%22character%22%3A11%7D%7D%5D%5D "Go to definition")
         - This specifies that the decimal point in numeric literals is represented by a comma instead of a period.

3. **Data Division**
   - **File Section**
     - Currently, there are no files defined in this section.
   - **Working-Storage Section**
     - **Fields**
       - [`WS-M`](command:_github.copilot.openSymbolFromReferences?%5B%22WS-M%22%2C%5B%7B%22uri%22%3A%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2Fworkspaces%2Fcobol-apps%2FCohen%20Sutherland%2FcohenSutherland.cob%22%2C%22external%22%3A%22file%3A%2F%2F%2Fworkspaces%2Fcobol-apps%2FCohen%2520Sutherland%2FcohenSutherland.cob%22%2C%22path%22%3A%22%2Fworkspaces%2Fcobol-apps%2FCohen%20Sutherland%2FcohenSutherland.cob%22%2C%22scheme%22%3A%22file%22%7D%2C%22pos%22%3A%7B%22line%22%3A17%2C%22character%22%3A10%7D%7D%5D%5D "Go to definition") (PIC 9(09) VALUE ZEROES)
         - This field is used to store a numeric value initialized to zero.

4. **Procedure Division**
   - This section will contain the logic for the Cohen-Sutherland line clipping algorithm. The detailed implementation is not provided in the excerpt, but we can infer the following pseudo logic based on the purpose of the program:

#### Pseudo Logic

1. **Initialize Program**
   - Display a welcome message.
   - Initialize necessary variables and constants for the Cohen-Sutherland algorithm.

2. **Input Line Coordinates**
   - Accept the coordinates of the line to be clipped from the user.

3. **Input Clipping Window Coordinates**
   - Accept the coordinates of the clipping window from the user.

4. **Compute Outcodes**
   - For each endpoint of the line, compute the outcode based on the clipping window boundaries.

5. **Perform Clipping**
   - Use the outcodes to determine whether the line is completely inside, completely outside, or partially inside the clipping window.
   - If partially inside, compute the intersection points with the clipping window boundaries.

6. **Store Result**
   - Store the clipped line coordinates.

7. **Display Result**
   - Display the clipped line coordinates to the user.

8. **End Program**
   - Display a message indicating the end of the program.
   - Terminate the program.

#### Example Implementation (Pseudo Code)

```plaintext
START PROGRAM
  DISPLAY "Welcome to Cohen-Sutherland Line Clipping Algorithm in COBOL"
  
  DISPLAY "Enter the coordinates of the line (x1, y1, x2, y2):"
  ACCEPT WS-X1, WS-Y1, WS-X2, WS-Y2

  DISPLAY "Enter the coordinates of the clipping window (xmin, ymin, xmax, ymax):"
  ACCEPT WS-XMIN, WS-YMIN, WS-XMAX, WS-YMAX

  PERFORM COMPUTE-OUTCODES

  PERFORM CLIP-LINE

  DISPLAY "The clipped line coordinates are: " WS-CLIPPED-X1, WS-CLIPPED-Y1, WS-CLIPPED-X2, WS-CLIPPED-Y2

  DISPLAY "End of Program"
END PROGRAM

COMPUTE-OUTCODES SECTION.
  * Compute outcodes for the endpoints of the line
  * Based on the clipping window boundaries
  * Store the outcodes in WS-OUTCODE1 and WS-OUTCODE2
END SECTION.

CLIP-LINE SECTION.
  * Use the outcodes to determine the visibility of the line
  * If partially inside, compute intersection points
  * Store the clipped line coordinates in WS-CLIPPED-X1, WS-CLIPPED-Y1, WS-CLIPPED-X2, WS-CLIPPED-Y2
END SECTION.
```

This documentation provides an overview of the program structure, the purpose of each field, and a high-level pseudo logic of how the Cohen-Sutherland line clipping algorithm might be implemented.

sumitchohan:  generate detailed documentation for current code file including all methods, fields, properties and summary of their implementation including pseudo logic

GitHub Copilot: ### Detailed Documentation for [`PG13ALTCNT.COB`](command:_github.copilot.openRelativePath?%5B%7B%22scheme%22%3A%22file%22%2C%22authority%22%3A%22%22%2C%22path%22%3A%22%2Fworkspaces%2Fcobol-apps%2FContact%20List%20Writer%2FPG13ALTCNT.COB%22%2C%22query%22%3A%22%22%2C%22fragment%22%3A%22%22%7D%5D "/workspaces/cobol-apps/Contact List Writer/PG13ALTCNT.COB")

#### Summary
This COBOL program is designed for sequential file processing, specifically for updating contact records. The program includes various divisions and sections that handle the identification, environment configuration, file control, and data processing.

#### Divisions and Sections

1. **Identification Division**
   - **PROGRAM-ID**: [`PG13ALTCNT`](command:_github.copilot.openSymbolFromReferences?%5B%22PG13ALTCNT%22%2C%5B%7B%22uri%22%3A%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2Fworkspaces%2Fcobol-apps%2FContact%20List%20Writer%2FPG13ALTCNT.COB%22%2C%22external%22%3A%22file%3A%2F%2F%2Fworkspaces%2Fcobol-apps%2FContact%2520List%2520Writer%2FPG13ALTCNT.COB%22%2C%22path%22%3A%22%2Fworkspaces%2Fcobol-apps%2FContact%20List%20Writer%2FPG13ALTCNT.COB%22%2C%22scheme%22%3A%22file%22%7D%2C%22pos%22%3A%7B%22line%22%3A7%2C%22character%22%3A19%7D%7D%5D%5D "Go to definition")
     - This is the unique identifier for the program.
   - **Author**: Raphael Frei
   - **Date**: 19/06/2022
   - **Purpose**: Sequential File Processing - Updating Contacts
   - **Update**: 20/06/2022 - Conversion to Module

2. **Environment Division**
   - **Configuration Section**
     - **SPECIAL-NAMES**
       - [`DECIMAL-POINT IS COMMA`](command:_github.copilot.openSymbolFromReferences?%5B%22DECIMAL-POINT%20IS%20COMMA%22%2C%5B%7B%22uri%22%3A%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2Fworkspaces%2Fcobol-apps%2FContact%20List%20Writer%2FPG13ALTCNT.COB%22%2C%22external%22%3A%22file%3A%2F%2F%2Fworkspaces%2Fcobol-apps%2FContact%2520List%2520Writer%2FPG13ALTCNT.COB%22%2C%22path%22%3A%22%2Fworkspaces%2Fcobol-apps%2FContact%20List%20Writer%2FPG13ALTCNT.COB%22%2C%22scheme%22%3A%22file%22%7D%2C%22pos%22%3A%7B%22line%22%3A13%2C%22character%22%3A12%7D%7D%5D%5D "Go to definition")
         - This specifies that the decimal point in numeric literals is represented by a comma instead of a period.

3. **Input-Output Section**
   - **File-Control**
     - **SELECT CONTATOS ASSIGN TO**
       - This statement is used to assign the logical file name [`CONTATOS`](command:_github.copilot.openSymbolFromReferences?%5B%22CONTATOS%22%2C%5B%7B%22uri%22%3A%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2Fworkspaces%2Fcobol-apps%2FContact%20List%20Writer%2FPG13ALTCNT.COB%22%2C%22external%22%3A%22file%3A%2F%2F%2Fworkspaces%2Fcobol-apps%2FContact%2520List%2520Writer%2FPG13ALTCNT.COB%22%2C%22path%22%3A%22%2Fworkspaces%2Fcobol-apps%2FContact%20List%20Writer%2FPG13ALTCNT.COB%22%2C%22scheme%22%3A%22file%22%7D%2C%22pos%22%3A%7B%22line%22%3A17%2C%22character%22%3A19%7D%7D%5D%5D "Go to definition") to a physical file. The full file control details are not provided in the excerpt.

4. **Data Division**
   - This section will define the data structures and variables used in the program. The detailed implementation is not provided in the excerpt, but we can infer the following based on the purpose of the program:

#### Pseudo Logic

1. **Initialize Program**
   - Display a welcome message.
   - Initialize necessary variables and file handles.

2. **Open Files**
   - Open the input file for reading contact records.
   - Open the output file for writing updated contact records.

3. **Read and Process Records**
   - Read each contact record from the input file.
   - For each record, perform the necessary updates.
   - Write the updated record to the output file.

4. **Close Files**
   - Close both the input and output files.

5. **End Program**
   - Display a message indicating the end of the program.
   - Terminate the program.

#### Example Implementation (Pseudo Code)

```plaintext
START PROGRAM
  DISPLAY "Welcome to the Contact Update Program"

  OPEN INPUT CONTATOS-IN
  OPEN OUTPUT CONTATOS-OUT

  PERFORM UNTIL END-OF-FILE
    READ CONTATOS-IN INTO WS-CONTACT-RECORD
    IF NOT END-OF-FILE
      PERFORM UPDATE-CONTACT
      WRITE WS-CONTACT-RECORD TO CONTATOS-OUT
    END-IF
  END-PERFORM

  CLOSE CONTATOS-IN
  CLOSE CONTATOS-OUT

  DISPLAY "End of Program"
END PROGRAM

UPDATE-CONTACT SECTION.
  * Perform necessary updates to WS-CONTACT-RECORD
  * This might include modifying fields based on certain conditions
END SECTION.
```

#### Fields and Variables (Inferred)

- **WS-CONTACT-RECORD**: Working-storage field to hold a contact record read from the input file.
- **CONTATOS-IN**: Logical name for the input file containing contact records.
- **CONTATOS-OUT**: Logical name for the output file where updated contact records will be written.
- **END-OF-FILE**: Flag to indicate the end of the input file.

This documentation provides an overview of the program structure, the purpose of each section, and a high-level pseudo logic of how the contact update process might be implemented.

sumitchohan:  generate detailed documentation for current code file including all methods, fields, properties and summary of their implementation including pseudo logic
 

GitHub Copilot: ### Detailed Documentation for [`PG13CADCTT.COB`](command:_github.copilot.openRelativePath?%5B%7B%22scheme%22%3A%22file%22%2C%22authority%22%3A%22%22%2C%22path%22%3A%22%2Fworkspaces%2Fcobol-apps%2FContact%20List%20Writer%2FPG13CADCTT.COB%22%2C%22query%22%3A%22%22%2C%22fragment%22%3A%22%22%7D%5D "/workspaces/cobol-apps/Contact List Writer/PG13CADCTT.COB")

#### Summary
This COBOL program is designed for sequential file processing, specifically for recording contact information. The program includes various divisions and sections that handle the identification, environment configuration, file control, and data processing.

#### Divisions and Sections

1. **Identification Division**
   - **PROGRAM-ID**: [`PG13CADCTT`](command:_github.copilot.openSymbolFromReferences?%5B%22PG13CADCTT%22%2C%5B%7B%22uri%22%3A%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2Fworkspaces%2Fcobol-apps%2FContact%20List%20Writer%2FPG13CADCTT.COB%22%2C%22external%22%3A%22file%3A%2F%2F%2Fworkspaces%2Fcobol-apps%2FContact%2520List%2520Writer%2FPG13CADCTT.COB%22%2C%22path%22%3A%22%2Fworkspaces%2Fcobol-apps%2FContact%20List%20Writer%2FPG13CADCTT.COB%22%2C%22scheme%22%3A%22file%22%7D%2C%22pos%22%3A%7B%22line%22%3A8%2C%22character%22%3A19%7D%7D%5D%5D "Go to definition")
     - This is the unique identifier for the program.
   - **Author**: Raphael Frei
   - **Date**: 19/06/2022
   - **Purpose**: Gravação Sequencial - Cadastro de Contatos (Sequential Recording - Contact Registration)
   - **Update**: 20/06/2022 - Conversão para Módulo (Conversion to Module)

2. **Environment Division**
   - **Configuration Section**
     - **SPECIAL-NAMES**
       - [`DECIMAL-POINT IS COMMA`](command:_github.copilot.openSymbolFromReferences?%5B%22DECIMAL-POINT%20IS%20COMMA%22%2C%5B%7B%22uri%22%3A%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2Fworkspaces%2Fcobol-apps%2FContact%20List%20Writer%2FPG13CADCTT.COB%22%2C%22external%22%3A%22file%3A%2F%2F%2Fworkspaces%2Fcobol-apps%2FContact%2520List%2520Writer%2FPG13CADCTT.COB%22%2C%22path%22%3A%22%2Fworkspaces%2Fcobol-apps%2FContact%20List%20Writer%2FPG13CADCTT.COB%22%2C%22scheme%22%3A%22file%22%7D%2C%22pos%22%3A%7B%22line%22%3A14%2C%22character%22%3A12%7D%7D%5D%5D "Go to definition")
         - This specifies that the decimal point in numeric literals is represented by a comma instead of a period.

3. **Input-Output Section**
   - **File-Control**
     - This section will define the file control entries for the program. The detailed implementation is not provided in the excerpt, but we can infer the following based on the purpose of the program:

#### Pseudo Logic

1. **Initialize Program**
   - Display a welcome message.
   - Initialize necessary variables and file handles.

2. **Open Files**
   - Open the input file for reading contact records.
   - Open the output file for writing new contact records.

3. **Read and Process Records**
   - Read each contact record from the input file.
   - For each record, perform the necessary processing.
   - Write the processed record to the output file.

4. **Close Files**
   - Close both the input and output files.

5. **End Program**
   - Display a message indicating the end of the program.
   - Terminate the program.

#### Example Implementation (Pseudo Code)

```plaintext
START PROGRAM
  DISPLAY "Welcome to the Contact Registration Program"

  OPEN INPUT CONTACTS-IN
  OPEN OUTPUT CONTACTS-OUT

  PERFORM UNTIL END-OF-FILE
    READ CONTACTS-IN INTO WS-CONTACT-RECORD
    IF NOT END-OF-FILE
      PERFORM PROCESS-CONTACT
      WRITE WS-CONTACT-RECORD TO CONTACTS-OUT
    END-IF
  END-PERFORM

  CLOSE CONTACTS-IN
  CLOSE CONTACTS-OUT

  DISPLAY "End of Program"
END PROGRAM

PROCESS-CONTACT SECTION.
  * Perform necessary processing on WS-CONTACT-RECORD
  * This might include validating and formatting fields
END SECTION.
```

#### Fields and Variables (Inferred)

- **WS-CONTACT-RECORD**: Working-storage field to hold a contact record read from the input file.
- **CONTACTS-IN**: Logical name for the input file containing contact records.
- **CONTACTS-OUT**: Logical name for the output file where new contact records will be written.
- **END-OF-FILE**: Flag to indicate the end of the input file.

This documentation provides an overview of the program structure, the purpose of each section, and a high-level pseudo logic of how the contact registration process might be implemented.

sumitchohan:  generate detailed documentation for current code file including all methods, fields, properties and summary of their implementation including pseudo logic

GitHub Copilot: ### Detailed Documentation for [`PG13CONSCTT.COB`](command:_github.copilot.openRelativePath?%5B%7B%22scheme%22%3A%22file%22%2C%22authority%22%3A%22%22%2C%22path%22%3A%22%2Fworkspaces%2Fcobol-apps%2FContact%20List%20Writer%2FPG13CONSCTT.COB%22%2C%22query%22%3A%22%22%2C%22fragment%22%3A%22%22%7D%5D "/workspaces/cobol-apps/Contact List Writer/PG13CONSCTT.COB")

#### Summary
This COBOL program is designed for sequential file processing, specifically for querying contact information. The program includes various divisions and sections that handle the identification, environment configuration, file control, and data processing.

#### Divisions and Sections

1. **Identification Division**
   - **PROGRAM-ID**: [`PG13CONSCTT`](command:_github.copilot.openSymbolFromReferences?%5B%22PG13CONSCTT%22%2C%5B%7B%22uri%22%3A%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2Fworkspaces%2Fcobol-apps%2FContact%20List%20Writer%2FPG13CONSCTT.COB%22%2C%22external%22%3A%22file%3A%2F%2F%2Fworkspaces%2Fcobol-apps%2FContact%2520List%2520Writer%2FPG13CONSCTT.COB%22%2C%22path%22%3A%22%2Fworkspaces%2Fcobol-apps%2FContact%20List%20Writer%2FPG13CONSCTT.COB%22%2C%22scheme%22%3A%22file%22%7D%2C%22pos%22%3A%7B%22line%22%3A7%2C%22character%22%3A19%7D%7D%5D%5D "Go to definition")
     - This is the unique identifier for the program.
   - **Author**: Raphael Frei
   - **Date**: 19/06/2022
   - **Purpose**: Gravação Sequencial - Consulta de Contatos (Sequential Recording - Contact Query)
   - **Update**: 20/06/2022 - Conversão para Módulo (Conversion to Module)

2. **Environment Division**
   - **Configuration Section**
     - **SPECIAL-NAMES**
       - [`DECIMAL-POINT IS COMMA`](command:_github.copilot.openSymbolFromReferences?%5B%22DECIMAL-POINT%20IS%20COMMA%22%2C%5B%7B%22uri%22%3A%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2Fworkspaces%2Fcobol-apps%2FContact%20List%20Writer%2FPG13CONSCTT.COB%22%2C%22external%22%3A%22file%3A%2F%2F%2Fworkspaces%2Fcobol-apps%2FContact%2520List%2520Writer%2FPG13CONSCTT.COB%22%2C%22path%22%3A%22%2Fworkspaces%2Fcobol-apps%2FContact%20List%20Writer%2FPG13CONSCTT.COB%22%2C%22scheme%22%3A%22file%22%7D%2C%22pos%22%3A%7B%22line%22%3A13%2C%22character%22%3A12%7D%7D%5D%5D "Go to definition")
         - This specifies that the decimal point in numeric literals is represented by a comma instead of a period.

3. **Input-Output Section**
   - **File-Control**
     - **SELECT CONTATOS ASSIGN TO**
       - This statement is used to assign the logical file name [`CONTATOS`](command:_github.copilot.openSymbolFromReferences?%5B%22CONTATOS%22%2C%5B%7B%22uri%22%3A%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2Fworkspaces%2Fcobol-apps%2FContact%20List%20Writer%2FPG13CONSCTT.COB%22%2C%22external%22%3A%22file%3A%2F%2F%2Fworkspaces%2Fcobol-apps%2FContact%2520List%2520Writer%2FPG13CONSCTT.COB%22%2C%22path%22%3A%22%2Fworkspaces%2Fcobol-apps%2FContact%20List%20Writer%2FPG13CONSCTT.COB%22%2C%22scheme%22%3A%22file%22%7D%2C%22pos%22%3A%7B%22line%22%3A17%2C%22character%22%3A19%7D%7D%5D%5D "Go to definition") to a physical file. The full file control details are not provided in the excerpt.

4. **Data Division**
   - This section will define the data structures and variables used in the program. The detailed implementation is not provided in the excerpt, but we can infer the following based on the purpose of the program:

#### Pseudo Logic

1. **Initialize Program**
   - Display a welcome message.
   - Initialize necessary variables and file handles.

2. **Open Files**
   - Open the input file for reading contact records.

3. **Read and Process Records**
   - Read each contact record from the input file.
   - For each record, perform the necessary query operations.
   - Display the queried contact information.

4. **Close Files**
   - Close the input file.

5. **End Program**
   - Display a message indicating the end of the program.
   - Terminate the program.

#### Example Implementation (Pseudo Code)

```plaintext
START PROGRAM
  DISPLAY "Welcome to the Contact Query Program"

  OPEN INPUT CONTATOS-IN

  PERFORM UNTIL END-OF-FILE
    READ CONTATOS-IN INTO WS-CONTACT-RECORD
    IF NOT END-OF-FILE
      PERFORM QUERY-CONTACT
      DISPLAY WS-CONTACT-RECORD
    END-IF
  END-PERFORM

  CLOSE CONTATOS-IN

  DISPLAY "End of Program"
END PROGRAM

QUERY-CONTACT SECTION.
  * Perform necessary query operations on WS-CONTACT-RECORD
  * This might include searching for specific fields or values
END SECTION.
```

#### Fields and Variables (Inferred)

- **WS-CONTACT-RECORD**: Working-storage field to hold a contact record read from the input file.
- **CONTATOS-IN**: Logical name for the input file containing contact records.
- **END-OF-FILE**: Flag to indicate the end of the input file.

This documentation provides an overview of the program structure, the purpose of each section, and a high-level pseudo logic of how the contact query process might be implemented.

sumitchohan:  generate detailed documentation for current code file including all methods, fields, properties and summary of their implementation including pseudo logic

GitHub Copilot: ### Detailed Documentation for [`PG13DELCNT.COB`](command:_github.copilot.openRelativePath?%5B%7B%22scheme%22%3A%22file%22%2C%22authority%22%3A%22%22%2C%22path%22%3A%22%2Fworkspaces%2Fcobol-apps%2FContact%20List%20Writer%2FPG13DELCNT.COB%22%2C%22query%22%3A%22%22%2C%22fragment%22%3A%22%22%7D%5D "/workspaces/cobol-apps/Contact List Writer/PG13DELCNT.COB")

#### Summary
This COBOL program is designed for sequential file processing, specifically for deleting contact records. The program includes various divisions and sections that handle the identification, environment configuration, file control, and data processing.

#### Divisions and Sections

1. **Identification Division**
   - **PROGRAM-ID**: [`PG13DELCNT`](command:_github.copilot.openSymbolFromReferences?%5B%22PG13DELCNT%22%2C%5B%7B%22uri%22%3A%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2Fworkspaces%2Fcobol-apps%2FContact%20List%20Writer%2FPG13DELCNT.COB%22%2C%22external%22%3A%22file%3A%2F%2F%2Fworkspaces%2Fcobol-apps%2FContact%2520List%2520Writer%2FPG13DELCNT.COB%22%2C%22path%22%3A%22%2Fworkspaces%2Fcobol-apps%2FContact%20List%20Writer%2FPG13DELCNT.COB%22%2C%22scheme%22%3A%22file%22%7D%2C%22pos%22%3A%7B%22line%22%3A7%2C%22character%22%3A19%7D%7D%5D%5D "Go to definition")
     - This is the unique identifier for the program.
   - **Author**: Raphael Frei
   - **Date**: 19/06/2022
   - **Purpose**: Gravação Sequencial - Deletar Contatos (Sequential Recording - Delete Contacts)
   - **Update**: 20/06/2022 - Conversão para Módulo (Conversion to Module)

2. **Environment Division**
   - **Configuration Section**
     - **SPECIAL-NAMES**
       - [`DECIMAL-POINT IS COMMA`](command:_github.copilot.openSymbolFromReferences?%5B%22DECIMAL-POINT%20IS%20COMMA%22%2C%5B%7B%22uri%22%3A%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2Fworkspaces%2Fcobol-apps%2FContact%20List%20Writer%2FPG13DELCNT.COB%22%2C%22external%22%3A%22file%3A%2F%2F%2Fworkspaces%2Fcobol-apps%2FContact%2520List%2520Writer%2FPG13DELCNT.COB%22%2C%22path%22%3A%22%2Fworkspaces%2Fcobol-apps%2FContact%20List%20Writer%2FPG13DELCNT.COB%22%2C%22scheme%22%3A%22file%22%7D%2C%22pos%22%3A%7B%22line%22%3A13%2C%22character%22%3A12%7D%7D%5D%5D "Go to definition")
         - This specifies that the decimal point in numeric literals is represented by a comma instead of a period.

3. **Input-Output Section**
   - **File-Control**
     - **SELECT CONTATOS ASSIGN TO**
       - This statement is used to assign the logical file name [`CONTATOS`](command:_github.copilot.openSymbolFromReferences?%5B%22CONTATOS%22%2C%5B%7B%22uri%22%3A%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2Fworkspaces%2Fcobol-apps%2FContact%20List%20Writer%2FPG13DELCNT.COB%22%2C%22external%22%3A%22file%3A%2F%2F%2Fworkspaces%2Fcobol-apps%2FContact%2520List%2520Writer%2FPG13DELCNT.COB%22%2C%22path%22%3A%22%2Fworkspaces%2Fcobol-apps%2FContact%20List%20Writer%2FPG13DELCNT.COB%22%2C%22scheme%22%3A%22file%22%7D%2C%22pos%22%3A%7B%22line%22%3A17%2C%22character%22%3A19%7D%7D%5D%5D "Go to definition") to a physical file. The full file control details are not provided in the excerpt.

4. **Data Division**
   - This section will define the data structures and variables used in the program. The detailed implementation is not provided in the excerpt, but we can infer the following based on the purpose of the program:

#### Pseudo Logic

1. **Initialize Program**
   - Display a welcome message.
   - Initialize necessary variables and file handles.

2. **Open Files**
   - Open the input file for reading contact records.
   - Open the output file for writing updated contact records (excluding deleted ones).

3. **Read and Process Records**
   - Read each contact record from the input file.
   - For each record, check if it should be deleted.
   - If the record should not be deleted, write it to the output file.

4. **Close Files**
   - Close both the input and output files.

5. **End Program**
   - Display a message indicating the end of the program.
   - Terminate the program.

#### Example Implementation (Pseudo Code)

```plaintext
START PROGRAM
  DISPLAY "Welcome to the Contact Deletion Program"

  OPEN INPUT CONTACTS-IN
  OPEN OUTPUT CONTACTS-OUT

  PERFORM UNTIL END-OF-FILE
    READ CONTACTS-IN INTO WS-CONTACT-RECORD
    IF NOT END-OF-FILE
      PERFORM CHECK-DELETE-CONTACT
      IF NOT WS-DELETE-FLAG
        WRITE WS-CONTACT-RECORD TO CONTACTS-OUT
      END-IF
    END-IF
  END-PERFORM

  CLOSE CONTACTS-IN
  CLOSE CONTACTS-OUT

  DISPLAY "End of Program"
END PROGRAM

CHECK-DELETE-CONTACT SECTION.
  * Check if the contact record should be deleted
  * Set WS-DELETE-FLAG to true if the record should be deleted
END SECTION.
```

#### Fields and Variables (Inferred)

- **WS-CONTACT-RECORD**: Working-storage field to hold a contact record read from the input file.
- **CONTACTS-IN**: Logical name for the input file containing contact records.
- **CONTACTS-OUT**: Logical name for the output file where updated contact records will be written.
- **END-OF-FILE**: Flag to indicate the end of the input file.
- **WS-DELETE-FLAG**: Working-storage field to indicate whether a contact record should be deleted.

This documentation provides an overview of the program structure, the purpose of each section, and a high-level pseudo logic of how the contact deletion process might be implemented.

sumitchohan:  generate detailed documentation for current code file including all methods, fields, properties and summary of their implementation including pseudo logic

GitHub Copilot: ### Detailed Documentation for [`PG13LSCNTT.COB`](command:_github.copilot.openRelativePath?%5B%7B%22scheme%22%3A%22file%22%2C%22authority%22%3A%22%22%2C%22path%22%3A%22%2Fworkspaces%2Fcobol-apps%2FContact%20List%20Writer%2FPG13LSCNTT.COB%22%2C%22query%22%3A%22%22%2C%22fragment%22%3A%22%22%7D%5D "/workspaces/cobol-apps/Contact List Writer/PG13LSCNTT.COB")

#### Summary
This COBOL program is designed for sequential file processing, specifically for reading contact information. The program includes various divisions and sections that handle the identification, environment configuration, file control, and data processing.

#### Divisions and Sections

1. **Identification Division**
   - **PROGRAM-ID**: [`PG13LSCNTT`](command:_github.copilot.openSymbolFromReferences?%5B%22PG13LSCNTT%22%2C%5B%7B%22uri%22%3A%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2Fworkspaces%2Fcobol-apps%2FContact%20List%20Writer%2FPG13LSCNTT.COB%22%2C%22external%22%3A%22file%3A%2F%2F%2Fworkspaces%2Fcobol-apps%2FContact%2520List%2520Writer%2FPG13LSCNTT.COB%22%2C%22path%22%3A%22%2Fworkspaces%2Fcobol-apps%2FContact%20List%20Writer%2FPG13LSCNTT.COB%22%2C%22scheme%22%3A%22file%22%7D%2C%22pos%22%3A%7B%22line%22%3A7%2C%22character%22%3A19%7D%7D%5D%5D "Go to definition")
     - This is the unique identifier for the program.
   - **Author**: Raphael Frei
   - **Date**: 19/06/2022
   - **Purpose**: Gravação Sequencial - Leitura de Contatos (Sequential Recording - Reading Contacts)
   - **Update**: 20/06/2022 - Conversão para Módulo (Conversion to Module)

2. **Environment Division**
   - **Configuration Section**
     - **SPECIAL-NAMES**
       - [`DECIMAL-POINT IS COMMA`](command:_github.copilot.openSymbolFromReferences?%5B%22DECIMAL-POINT%20IS%20COMMA%22%2C%5B%7B%22uri%22%3A%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2Fworkspaces%2Fcobol-apps%2FContact%20List%20Writer%2FPG13LSCNTT.COB%22%2C%22external%22%3A%22file%3A%2F%2F%2Fworkspaces%2Fcobol-apps%2FContact%2520List%2520Writer%2FPG13LSCNTT.COB%22%2C%22path%22%3A%22%2Fworkspaces%2Fcobol-apps%2FContact%20List%20Writer%2FPG13LSCNTT.COB%22%2C%22scheme%22%3A%22file%22%7D%2C%22pos%22%3A%7B%22line%22%3A13%2C%22character%22%3A12%7D%7D%5D%5D "Go to definition")
         - This specifies that the decimal point in numeric literals is represented by a comma instead of a period.

3. **Input-Output Section**
   - **File-Control**
     - **SELECT CONTATOS ASSIGN TO**
       - This statement is used to assign the logical file name [`CONTATOS`](command:_github.copilot.openSymbolFromReferences?%5B%22CONTATOS%22%2C%5B%7B%22uri%22%3A%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2Fworkspaces%2Fcobol-apps%2FContact%20List%20Writer%2FPG13LSCNTT.COB%22%2C%22external%22%3A%22file%3A%2F%2F%2Fworkspaces%2Fcobol-apps%2FContact%2520List%2520Writer%2FPG13LSCNTT.COB%22%2C%22path%22%3A%22%2Fworkspaces%2Fcobol-apps%2FContact%20List%20Writer%2FPG13LSCNTT.COB%22%2C%22scheme%22%3A%22file%22%7D%2C%22pos%22%3A%7B%22line%22%3A17%2C%22character%22%3A19%7D%7D%5D%5D "Go to definition") to a physical file. The full file control details are not provided in the excerpt.

4. **Data Division**
   - This section will define the data structures and variables used in the program. The detailed implementation is not provided in the excerpt, but we can infer the following based on the purpose of the program:

#### Pseudo Logic

1. **Initialize Program**
   - Display a welcome message.
   - Initialize necessary variables and file handles.

2. **Open Files**
   - Open the input file for reading contact records.

3. **Read and Process Records**
   - Read each contact record from the input file.
   - For each record, perform the necessary processing.
   - Display the contact information.

4. **Close Files**
   - Close the input file.

5. **End Program**
   - Display a message indicating the end of the program.
   - Terminate the program.

#### Example Implementation (Pseudo Code)

```plaintext
START PROGRAM
  DISPLAY "Welcome to the Contact Reading Program"

  OPEN INPUT CONTACTS-IN

  PERFORM UNTIL END-OF-FILE
    READ CONTACTS-IN INTO WS-CONTACT-RECORD
    IF NOT END-OF-FILE
      PERFORM PROCESS-CONTACT
      DISPLAY WS-CONTACT-RECORD
    END-IF
  END-PERFORM

  CLOSE CONTACTS-IN

  DISPLAY "End of Program"
END PROGRAM

PROCESS-CONTACT SECTION.
  * Perform necessary processing on WS-CONTACT-RECORD
  * This might include validating and formatting fields
END SECTION.
```

#### Fields and Variables (Inferred)

- **WS-CONTACT-RECORD**: Working-storage field to hold a contact record read from the input file.
- **CONTACTS-IN**: Logical name for the input file containing contact records.
- **END-OF-FILE**: Flag to indicate the end of the input file.

This documentation provides an overview of the program structure, the purpose of each section, and a high-level pseudo logic of how the contact reading process might be implemented.

sumitchohan:  generate detailed documentation for current code file including all methods, fields, properties and summary of their implementation including pseudo logic

GitHub Copilot: ### Detailed Documentation for [`PG13MENU.cbl`](command:_github.copilot.openRelativePath?%5B%7B%22scheme%22%3A%22file%22%2C%22authority%22%3A%22%22%2C%22path%22%3A%22%2Fworkspaces%2Fcobol-apps%2FContact%20List%20Writer%2FPG13MENU.cbl%22%2C%22query%22%3A%22%22%2C%22fragment%22%3A%22%22%7D%5D "/workspaces/cobol-apps/Contact List Writer/PG13MENU.cbl")

#### Summary
This COBOL program serves as the main menu for the contact registration system. It provides options for the user to navigate through different functionalities such as adding, querying, and deleting contacts.

#### Divisions and Sections

1. **Identification Division**
   - **PROGRAM-ID**: [`PG13MENU`](command:_github.copilot.openSymbolFromReferences?%5B%22PG13MENU%22%2C%5B%7B%22uri%22%3A%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2Fworkspaces%2Fcobol-apps%2FContact%20List%20Writer%2FPG13MENU.cbl%22%2C%22external%22%3A%22file%3A%2F%2F%2Fworkspaces%2Fcobol-apps%2FContact%2520List%2520Writer%2FPG13MENU.cbl%22%2C%22path%22%3A%22%2Fworkspaces%2Fcobol-apps%2FContact%20List%20Writer%2FPG13MENU.cbl%22%2C%22scheme%22%3A%22file%22%7D%2C%22pos%22%3A%7B%22line%22%3A6%2C%22character%22%3A19%7D%7D%5D%5D "Go to definition")
     - This is the unique identifier for the program.
   - **Author**: Raphael Frei
   - **Date**: 20/06/2022
   - **Purpose**: Menu Principal para o Cadastro de Contatos (Main Menu for Contact Registration)

2. **Data Division**
   - **File Section**
     - This section is reserved for file declarations. No files are declared in the provided excerpt.

   - **Working-Storage Section**
     - **WS-COM-AREA**
       - **WS-MENSAGEM**: [`PIC X(20)`](command:_github.copilot.openSymbolFromReferences?%5B%22PIC%20X(20)%22%2C%5B%7B%22uri%22%3A%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2Fworkspaces%2Fcobol-apps%2FContact%20List%20Writer%2FPG13MENU.cbl%22%2C%22external%22%3A%22file%3A%2F%2F%2Fworkspaces%2Fcobol-apps%2FContact%2520List%2520Writer%2FPG13MENU.cbl%22%2C%22path%22%3A%22%2Fworkspaces%2Fcobol-apps%2FContact%20List%20Writer%2FPG13MENU.cbl%22%2C%22scheme%22%3A%22file%22%7D%2C%22pos%22%3A%7B%22line%22%3A12%2C%22character%22%3A43%7D%7D%5D%5D "Go to definition")
         - A 20-character field used to store messages.
     - **WS-OPCAO**: [`PIC X`](command:_github.copilot.openSymbolFromReferences?%5B%22PIC%20X%22%2C%5B%7B%22uri%22%3A%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2Fworkspaces%2Fcobol-apps%2FContact%20List%20Writer%2FPG13MENU.cbl%22%2C%22external%22%3A%22file%3A%2F%2F%2Fworkspaces%2Fcobol-apps%2FContact%2520List%2520Writer%2FPG13MENU.cbl%22%2C%22path%22%3A%22%2Fworkspaces%2Fcobol-apps%2FContact%20List%20Writer%2FPG13MENU.cbl%22%2C%22scheme%22%3A%22file%22%7D%2C%22pos%22%3A%7B%22line%22%3A12%2C%22character%22%3A43%7D%7D%5D%5D "Go to definition")
       - A single character field used to store the user's menu option selection.

3. **Procedure Division**
   - **P-START**
     - This is the starting point of the procedure division. The detailed implementation of the procedure is not provided in the excerpt.

#### Pseudo Logic

1. **Initialize Program**
   - Display a welcome message.
   - Display the main menu options.

2. **Get User Input**
   - Prompt the user to select an option from the menu.
   - Read the user's input into [`WS-OPCAO`](command:_github.copilot.openSymbolFromReferences?%5B%22WS-OPCAO%22%2C%5B%7B%22uri%22%3A%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2Fworkspaces%2Fcobol-apps%2FContact%20List%20Writer%2FPG13MENU.cbl%22%2C%22external%22%3A%22file%3A%2F%2F%2Fworkspaces%2Fcobol-apps%2FContact%2520List%2520Writer%2FPG13MENU.cbl%22%2C%22path%22%3A%22%2Fworkspaces%2Fcobol-apps%2FContact%20List%20Writer%2FPG13MENU.cbl%22%2C%22scheme%22%3A%22file%22%7D%2C%22pos%22%3A%7B%22line%22%3A11%2C%22character%22%3A10%7D%7D%5D%5D "Go to definition").

3. **Process User Selection**
   - Based on the value of [`WS-OPCAO`](command:_github.copilot.openSymbolFromReferences?%5B%22WS-OPCAO%22%2C%5B%7B%22uri%22%3A%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2Fworkspaces%2Fcobol-apps%2FContact%20List%20Writer%2FPG13MENU.cbl%22%2C%22external%22%3A%22file%3A%2F%2F%2Fworkspaces%2Fcobol-apps%2FContact%2520List%2520Writer%2FPG13MENU.cbl%22%2C%22path%22%3A%22%2Fworkspaces%2Fcobol-apps%2FContact%20List%20Writer%2FPG13MENU.cbl%22%2C%22scheme%22%3A%22file%22%7D%2C%22pos%22%3A%7B%22line%22%3A11%2C%22character%22%3A10%7D%7D%5D%5D "Go to definition"), perform the corresponding action:
     - If the user selects the option to add a contact, call the add contact module.
     - If the user selects the option to query contacts, call the query contact module.
     - If the user selects the option to delete a contact, call the delete contact module.
     - If the user selects the option to exit, terminate the program.

4. **End Program**
   - Display a message indicating the end of the program.
   - Terminate the program.

#### Example Implementation (Pseudo Code)

```plaintext
START PROGRAM
  DISPLAY "Welcome to the Contact Registration System"

  PERFORM UNTIL WS-OPCAO = 'E'
    DISPLAY "Main Menu"
    DISPLAY "1. Add Contact"
    DISPLAY "2. Query Contacts"
    DISPLAY "3. Delete Contact"
    DISPLAY "E. Exit"
    ACCEPT WS-OPCAO

    EVALUATE WS-OPCAO
      WHEN '1'
        PERFORM ADD-CONTACT
      WHEN '2'
        PERFORM QUERY-CONTACT
      WHEN '3'
        PERFORM DELETE-CONTACT
      WHEN 'E'
        DISPLAY "Exiting the program"
      WHEN OTHER
        DISPLAY "Invalid option, please try again"
    END-EVALUATE
  END-PERFORM

  DISPLAY "End of Program"
END PROGRAM

ADD-CONTACT SECTION.
  * Call the module to add a contact
END SECTION.

QUERY-CONTACT SECTION.
  * Call the module to query contacts
END SECTION.

DELETE-CONTACT SECTION.
  * Call the module to delete a contact
END SECTION.
```

#### Fields and Variables

- **WS-COM-AREA**
  - **WS-MENSAGEM**: [`PIC X(20)`](command:_github.copilot.openSymbolFromReferences?%5B%22PIC%20X(20)%22%2C%5B%7B%22uri%22%3A%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2Fworkspaces%2Fcobol-apps%2FContact%20List%20Writer%2FPG13MENU.cbl%22%2C%22external%22%3A%22file%3A%2F%2F%2Fworkspaces%2Fcobol-apps%2FContact%2520List%2520Writer%2FPG13MENU.cbl%22%2C%22path%22%3A%22%2Fworkspaces%2Fcobol-apps%2FContact%20List%20Writer%2FPG13MENU.cbl%22%2C%22scheme%22%3A%22file%22%7D%2C%22pos%22%3A%7B%22line%22%3A12%2C%22character%22%3A43%7D%7D%5D%5D "Go to definition")
    - A 20-character field used to store messages.
- **WS-OPCAO**: [`PIC X`](command:_github.copilot.openSymbolFromReferences?%5B%22PIC%20X%22%2C%5B%7B%22uri%22%3A%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2Fworkspaces%2Fcobol-apps%2FContact%20List%20Writer%2FPG13MENU.cbl%22%2C%22external%22%3A%22file%3A%2F%2F%2Fworkspaces%2Fcobol-apps%2FContact%2520List%2520Writer%2FPG13MENU.cbl%22%2C%22path%22%3A%22%2Fworkspaces%2Fcobol-apps%2FContact%20List%20Writer%2FPG13MENU.cbl%22%2C%22scheme%22%3A%22file%22%7D%2C%22pos%22%3A%7B%22line%22%3A12%2C%22character%22%3A43%7D%7D%5D%5D "Go to definition")
  - A single character field used to store the user's menu option selection.

This documentation provides an overview of the program structure, the purpose of each section, and a high-level pseudo logic of how the main menu functionality might be implemented.

sumitchohan:  generate detailed documentation for current code file including all methods, fields, properties and summary of their implementation including pseudo logic

GitHub Copilot: ### Detailed Documentation for [`CPF_CALC.COB`](command:_github.copilot.openRelativePath?%5B%7B%22scheme%22%3A%22file%22%2C%22authority%22%3A%22%22%2C%22path%22%3A%22%2Fworkspaces%2Fcobol-apps%2FCPF%20-%20Digits%20Verification%2FCPF_CALC.COB%22%2C%22query%22%3A%22%22%2C%22fragment%22%3A%22%22%7D%5D "/workspaces/cobol-apps/CPF - Digits Verification/CPF_CALC.COB")

#### Summary
This COBOL program is designed to verify the last two digits of a Brazilian CPF (Cadastro de Pessoas Físicas) number. The CPF is a unique identifier assigned to Brazilian citizens for tax purposes. The program includes various divisions and sections that handle the identification, environment configuration, file control, and data processing.

#### Divisions and Sections

1. **Identification Division**
   - **PROGRAM-ID**: [`CPF_CALC`](command:_github.copilot.openSymbolFromReferences?%5B%22CPF_CALC%22%2C%5B%7B%22uri%22%3A%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2Fworkspaces%2Fcobol-apps%2FCPF%20-%20Digits%20Verification%2FCPF_CALC.COB%22%2C%22external%22%3A%22file%3A%2F%2F%2Fworkspaces%2Fcobol-apps%2FCPF%2520-%2520Digits%2520Verification%2FCPF_CALC.COB%22%2C%22path%22%3A%22%2Fworkspaces%2Fcobol-apps%2FCPF%20-%20Digits%20Verification%2FCPF_CALC.COB%22%2C%22scheme%22%3A%22file%22%7D%2C%22pos%22%3A%7B%22line%22%3A6%2C%22character%22%3A19%7D%7D%5D%5D "Go to definition")
     - This is the unique identifier for the program.
   - **Author**: Raphael Frei
   - **Date**: 30/06/2022
   - **Purpose**: Verifica os dois digitos finais do CPF (Verify the last two digits of the CPF)

2. **Environment Division**
   - **Configuration Section**
     - **SPECIAL-NAMES**
       - [`DECIMAL-POINT IS COMMA`](command:_github.copilot.openSymbolFromReferences?%5B%22DECIMAL-POINT%20IS%20COMMA%22%2C%5B%7B%22uri%22%3A%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2Fworkspaces%2Fcobol-apps%2FCPF%20-%20Digits%20Verification%2FCPF_CALC.COB%22%2C%22external%22%3A%22file%3A%2F%2F%2Fworkspaces%2Fcobol-apps%2FCPF%2520-%2520Digits%2520Verification%2FCPF_CALC.COB%22%2C%22path%22%3A%22%2Fworkspaces%2Fcobol-apps%2FCPF%20-%20Digits%20Verification%2FCPF_CALC.COB%22%2C%22scheme%22%3A%22file%22%7D%2C%22pos%22%3A%7B%22line%22%3A11%2C%22character%22%3A11%7D%7D%5D%5D "Go to definition")
         - This specifies that the decimal point in numeric literals is represented by a comma instead of a period.

3. **Data Division**
   - **File Section**
     - This section is reserved for file declarations. No files are declared in the provided excerpt.

   - **Working-Storage Section**
     - **WS-CPF**: [`PIC 9(09) VALUE ZEROES`](command:_github.copilot.openSymbolFromReferences?%5B%22PIC%209(09)%20VALUE%20ZEROES%22%2C%5B%7B%22uri%22%3A%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2Fworkspaces%2Fcobol-apps%2FCPF%20-%20Digits%20Verification%2FCPF_CALC.COB%22%2C%22external%22%3A%22file%3A%2F%2F%2Fworkspaces%2Fcobol-apps%2FCPF%2520-%2520Digits%2520Verification%2FCPF_CALC.COB%22%2C%22path%22%3A%22%2Fworkspaces%2Fcobol-apps%2FCPF%20-%20Digits%20Verification%2FCPF_CALC.COB%22%2C%22scheme%22%3A%22file%22%7D%2C%22pos%22%3A%7B%22line%22%3A16%2C%22character%22%3A31%7D%7D%5D%5D "Go to definition")
       - A 9-digit numeric field initialized to zeroes, used to store the CPF number.
     - **WS-CPF-MSK**: [`PIC ZZZ.ZZZ.ZZ9`](command:_github.copilot.openSymbolFromReferences?%5B%22PIC%20ZZZ.ZZZ.ZZ9%22%2C%5B%7B%22uri%22%3A%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2Fworkspaces%2Fcobol-apps%2FCPF%20-%20Digits%20Verification%2FCPF_CALC.COB%22%2C%22external%22%3A%22file%3A%2F%2F%2Fworkspaces%2Fcobol-apps%2FCPF%2520-%2520Digits%2520Verification%2FCPF_CALC.COB%22%2C%22path%22%3A%22%2Fworkspaces%2Fcobol-apps%2FCPF%20-%20Digits%20Verification%2FCPF_CALC.COB%22%2C%22scheme%22%3A%22file%22%7D%2C%22pos%22%3A%7B%22line%22%3A17%2C%22character%22%3A31%7D%7D%5D%5D "Go to definition")
       - A formatted field used to display the CPF number with dots as separators.

#### Pseudo Logic

1. **Initialize Program**
   - Display a welcome message.
   - Initialize necessary variables.

2. **Input CPF Number**
   - Prompt the user to enter a 9-digit CPF number.
   - Read the user's input into [`WS-CPF`](command:_github.copilot.openSymbolFromReferences?%5B%22WS-CPF%22%2C%5B%7B%22uri%22%3A%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2Fworkspaces%2Fcobol-apps%2FCPF%20-%20Digits%20Verification%2FCPF_CALC.COB%22%2C%22external%22%3A%22file%3A%2F%2F%2Fworkspaces%2Fcobol-apps%2FCPF%2520-%2520Digits%2520Verification%2FCPF_CALC.COB%22%2C%22path%22%3A%22%2Fworkspaces%2Fcobol-apps%2FCPF%20-%20Digits%20Verification%2FCPF_CALC.COB%22%2C%22scheme%22%3A%22file%22%7D%2C%22pos%22%3A%7B%22line%22%3A16%2C%22character%22%3A10%7D%7D%5D%5D "Go to definition").

3. **Calculate Verification Digits**
   - Perform the calculation to determine the last two verification digits of the CPF.
   - Validate the calculated digits against the provided CPF.

4. **Display Results**
   - Format the CPF number using [`WS-CPF-MSK`](command:_github.copilot.openSymbolFromReferences?%5B%22WS-CPF-MSK%22%2C%5B%7B%22uri%22%3A%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2Fworkspaces%2Fcobol-apps%2FCPF%20-%20Digits%20Verification%2FCPF_CALC.COB%22%2C%22external%22%3A%22file%3A%2F%2F%2Fworkspaces%2Fcobol-apps%2FCPF%2520-%2520Digits%2520Verification%2FCPF_CALC.COB%22%2C%22path%22%3A%22%2Fworkspaces%2Fcobol-apps%2FCPF%20-%20Digits%20Verification%2FCPF_CALC.COB%22%2C%22scheme%22%3A%22file%22%7D%2C%22pos%22%3A%7B%22line%22%3A16%2C%22character%22%3A10%7D%7D%5D%5D "Go to definition").
   - Display the formatted CPF number and the verification result.

5. **End Program**
   - Display a message indicating the end of the program.
   - Terminate the program.

#### Example Implementation (Pseudo Code)

```plaintext
START PROGRAM
  DISPLAY "Welcome to the CPF Verification Program"

  DISPLAY "Enter the first 9 digits of the CPF:"
  ACCEPT WS-CPF

  PERFORM CALCULATE-VERIFICATION-DIGITS

  MOVE WS-CPF TO WS-CPF-MSK
  DISPLAY "Formatted CPF: " WS-CPF-MSK
  DISPLAY "Verification result: " WS-VERIFICATION-RESULT

  DISPLAY "End of Program"
END PROGRAM

CALCULATE-VERIFICATION-DIGITS SECTION.
  * Perform the calculation to determine the last two verification digits
  * Validate the calculated digits against the provided CPF
  * Set WS-VERIFICATION-RESULT to indicate whether the CPF is valid
END SECTION.
```

#### Fields and Variables

- **WS-CPF**: [`PIC 9(09) VALUE ZEROES`](command:_github.copilot.openSymbolFromReferences?%5B%22PIC%209(09)%20VALUE%20ZEROES%22%2C%5B%7B%22uri%22%3A%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2Fworkspaces%2Fcobol-apps%2FCPF%20-%20Digits%20Verification%2FCPF_CALC.COB%22%2C%22external%22%3A%22file%3A%2F%2F%2Fworkspaces%2Fcobol-apps%2FCPF%2520-%2520Digits%2520Verification%2FCPF_CALC.COB%22%2C%22path%22%3A%22%2Fworkspaces%2Fcobol-apps%2FCPF%20-%20Digits%20Verification%2FCPF_CALC.COB%22%2C%22scheme%22%3A%22file%22%7D%2C%22pos%22%3A%7B%22line%22%3A16%2C%22character%22%3A31%7D%7D%5D%5D "Go to definition")
  - A 9-digit numeric field used to store the CPF number.
- **WS-CPF-MSK**: [`PIC ZZZ.ZZZ.ZZ9`](command:_github.copilot.openSymbolFromReferences?%5B%22PIC%20ZZZ.ZZZ.ZZ9%22%2C%5B%7B%22uri%22%3A%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2Fworkspaces%2Fcobol-apps%2FCPF%20-%20Digits%20Verification%2FCPF_CALC.COB%22%2C%22external%22%3A%22file%3A%2F%2F%2Fworkspaces%2Fcobol-apps%2FCPF%2520-%2520Digits%2520Verification%2FCPF_CALC.COB%22%2C%22path%22%3A%22%2Fworkspaces%2Fcobol-apps%2FCPF%20-%20Digits%20Verification%2FCPF_CALC.COB%22%2C%22scheme%22%3A%22file%22%7D%2C%22pos%22%3A%7B%22line%22%3A17%2C%22character%22%3A31%7D%7D%5D%5D "Go to definition")
  - A formatted field used to display the CPF number with dots as separators.
- **WS-VERIFICATION-RESULT**: (Inferred)
  - A field used to store the result of the CPF verification process.

This documentation provides an overview of the program structure, the purpose of each section, and a high-level pseudo logic of how the CPF verification process might be implemented.

sumitchohan:  generate detailed documentation for current code file including all methods, fields, properties and summary of their implementation including pseudo logic

GitHub Copilot: ### Detailed Documentation for [`PRGCSV.cbl`](command:_github.copilot.openRelativePath?%5B%7B%22scheme%22%3A%22file%22%2C%22authority%22%3A%22%22%2C%22path%22%3A%22%2Fworkspaces%2Fcobol-apps%2FCSV%20Writer%2FPRGCSV.cbl%22%2C%22query%22%3A%22%22%2C%22fragment%22%3A%22%22%7D%5D "/workspaces/cobol-apps/CSV Writer/PRGCSV.cbl")

#### Summary
This COBOL program is designed to generate a CSV report. The program includes various divisions and sections that handle the identification, environment configuration, file control, and data processing.

#### Divisions and Sections

1. **Identification Division**
   - **PROGRAM-ID**: [`PRGCSV`](command:_github.copilot.openSymbolFromReferences?%5B%22PRGCSV%22%2C%5B%7B%22uri%22%3A%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2Fworkspaces%2Fcobol-apps%2FCSV%20Writer%2FPRGCSV.cbl%22%2C%22external%22%3A%22file%3A%2F%2F%2Fworkspaces%2Fcobol-apps%2FCSV%2520Writer%2FPRGCSV.cbl%22%2C%22path%22%3A%22%2Fworkspaces%2Fcobol-apps%2FCSV%20Writer%2FPRGCSV.cbl%22%2C%22scheme%22%3A%22file%22%7D%2C%22pos%22%3A%7B%22line%22%3A6%2C%22character%22%3A19%7D%7D%5D%5D "Go to definition")
     - This is the unique identifier for the program.
   - **Author**: Raphael Frei
   - **Date**: 15/07/2022
   - **Purpose**: Gerar Relatório CSV (Generate CSV Report)

2. **Environment Division**
   - **Configuration Section**
     - **SPECIAL-NAMES**
       - [`DECIMAL-POINT IS COMMA`](command:_github.copilot.openSymbolFromReferences?%5B%22DECIMAL-POINT%20IS%20COMMA%22%2C%5B%7B%22uri%22%3A%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2Fworkspaces%2Fcobol-apps%2FCSV%20Writer%2FPRGCSV.cbl%22%2C%22external%22%3A%22file%3A%2F%2F%2Fworkspaces%2Fcobol-apps%2FCSV%2520Writer%2FPRGCSV.cbl%22%2C%22path%22%3A%22%2Fworkspaces%2Fcobol-apps%2FCSV%20Writer%2FPRGCSV.cbl%22%2C%22scheme%22%3A%22file%22%7D%2C%22pos%22%3A%7B%22line%22%3A11%2C%22character%22%3A11%7D%7D%5D%5D "Go to definition")
         - This specifies that the decimal point in numeric literals is represented by a comma instead of a period.

3. **Input-Output Section**
   - **File-Control**
     - **SELECT CSV**
       - [`ASSIGN TO ".\CTT_CSV.CSV"`](command:_github.copilot.openSymbolFromReferences?%5B%22ASSIGN%20TO%20%5C%22.%5C%5CCTT_CSV.CSV%5C%22%22%2C%5B%7B%22uri%22%3A%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2Fworkspaces%2Fcobol-apps%2FCSV%20Writer%2FPRGCSV.cbl%22%2C%22external%22%3A%22file%3A%2F%2F%2Fworkspaces%2Fcobol-apps%2FCSV%2520Writer%2FPRGCSV.cbl%22%2C%22path%22%3A%22%2Fworkspaces%2Fcobol-apps%2FCSV%20Writer%2FPRGCSV.cbl%22%2C%22scheme%22%3A%22file%22%7D%2C%22pos%22%3A%7B%22line%22%3A15%2C%22character%22%3A30%7D%7D%5D%5D "Go to definition")
         - Specifies the file path for the CSV file.
       - [`ORGANIZATION IS LINE SEQUENTIAL`](command:_github.copilot.openSymbolFromReferences?%5B%22ORGANIZATION%20IS%20LINE%20SEQUENTIAL%22%2C%5B%7B%22uri%22%3A%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2Fworkspaces%2Fcobol-apps%2FCSV%20Writer%2FPRGCSV.cbl%22%2C%22external%22%3A%22file%3A%2F%2F%2Fworkspaces%2Fcobol-apps%2FCSV%2520Writer%2FPRGCSV.cbl%22%2C%22path%22%3A%22%2Fworkspaces%2Fcobol-apps%2FCSV%20Writer%2FPRGCSV.cbl%22%2C%22scheme%22%3A%22file%22%7D%2C%22pos%22%3A%7B%22line%22%3A16%2C%22character%22%3A54%7D%7D%5D%5D "Go to definition")
         - Indicates that the file is organized as a sequence of lines.

4. **Data Division**
   - **File Section**
     - This section is reserved for file declarations. The detailed implementation of the file structure is not provided in the excerpt.

   - **Working-Storage Section**
     - This section is reserved for working storage variables. The detailed implementation of the variables is not provided in the excerpt.

#### Pseudo Logic

1. **Initialize Program**
   - Display a welcome message.
   - Initialize necessary variables.

2. **Open CSV File**
   - Open the CSV file for writing.

3. **Write Data to CSV File**
   - Retrieve data to be written to the CSV file.
   - Format the data as CSV.
   - Write the formatted data to the CSV file.

4. **Close CSV File**
   - Close the CSV file after writing all data.

5. **End Program**
   - Display a message indicating the end of the program.
   - Terminate the program.

#### Example Implementation (Pseudo Code)

```plaintext
START PROGRAM
  DISPLAY "Welcome to the CSV Report Generator"

  OPEN OUTPUT CSV

  PERFORM WRITE-CSV-DATA

  CLOSE CSV

  DISPLAY "CSV Report Generation Completed"
  DISPLAY "End of Program"
END PROGRAM

WRITE-CSV-DATA SECTION.
  * Retrieve data to be written to the CSV file
  * Format the data as CSV
  * Write the formatted data to the CSV file
END SECTION.
```

#### Fields and Variables

- **CSV File**
  - **SELECT CSV**: [`ASSIGN TO ".\CTT_CSV.CSV"`](command:_github.copilot.openSymbolFromReferences?%5B%22ASSIGN%20TO%20%5C%22.%5C%5CCTT_CSV.CSV%5C%22%22%2C%5B%7B%22uri%22%3A%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2Fworkspaces%2Fcobol-apps%2FCSV%20Writer%2FPRGCSV.cbl%22%2C%22external%22%3A%22file%3A%2F%2F%2Fworkspaces%2Fcobol-apps%2FCSV%2520Writer%2FPRGCSV.cbl%22%2C%22path%22%3A%22%2Fworkspaces%2Fcobol-apps%2FCSV%20Writer%2FPRGCSV.cbl%22%2C%22scheme%22%3A%22file%22%7D%2C%22pos%22%3A%7B%22line%22%3A15%2C%22character%22%3A30%7D%7D%5D%5D "Go to definition")
    - Specifies the file path for the CSV file.
  - **ORGANIZATION IS LINE SEQUENTIAL**
    - Indicates that the file is organized as a sequence of lines.

This documentation provides an overview of the program structure, the purpose of each section, and a high-level pseudo logic of how the CSV report generation process might be implemented.

sumitchohan:  generate detailed documentation for current code file including all methods, fields, properties and summary of their implementation including pseudo logic

GitHub Copilot: ### Detailed Documentation for [`DECISCOMMA.cbl`](command:_github.copilot.openRelativePath?%5B%7B%22scheme%22%3A%22file%22%2C%22authority%22%3A%22%22%2C%22path%22%3A%22%2Fworkspaces%2Fcobol-apps%2FDec%20Is%20Comma%2FDECISCOMMA.cbl%22%2C%22query%22%3A%22%22%2C%22fragment%22%3A%22%22%7D%5D "/workspaces/cobol-apps/Dec Is Comma/DECISCOMMA.cbl")

#### Summary
This COBOL program is designed to handle numeric data where the decimal point is represented by a comma instead of a period. The program includes various divisions and sections that handle the identification, environment configuration, file control, and data processing.

#### Divisions and Sections

1. **Identification Division**
   - **PROGRAM-ID**: `DECISCOMMA`
     - This is the unique identifier for the program.
   - **Author**: [Author's Name]
   - **Date**: [Date]
   - **Purpose**: Handle numeric data with a comma as the decimal point.

2. **Environment Division**
   - **Configuration Section**
     - **SPECIAL-NAMES**
       - [`DECIMAL-POINT IS COMMA`](command:_github.copilot.openSymbolFromReferences?%5B%22DECIMAL-POINT%20IS%20COMMA%22%2C%5B%7B%22uri%22%3A%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2Fworkspaces%2Fcobol-apps%2FDec%20Is%20Comma%2FDECISCOMMA.cbl%22%2C%22external%22%3A%22file%3A%2F%2F%2Fworkspaces%2Fcobol-apps%2FDec%2520Is%2520Comma%2FDECISCOMMA.cbl%22%2C%22path%22%3A%22%2Fworkspaces%2Fcobol-apps%2FDec%20Is%20Comma%2FDECISCOMMA.cbl%22%2C%22scheme%22%3A%22file%22%7D%2C%22pos%22%3A%7B%22line%22%3A3%2C%22character%22%3A11%7D%7D%5D%5D "Go to definition")
         - This specifies that the decimal point in numeric literals is represented by a comma instead of a period.

3. **Data Division**
   - **File Section**
     - This section is reserved for file declarations. No files are declared in the provided excerpt.

   - **Working-Storage Section**
     - This section is reserved for working storage variables. The detailed implementation of the variables is not provided in the excerpt.

4. **Procedure Division**
   - This section contains the logic of the program. The detailed implementation is not provided in the excerpt.

#### Pseudo Logic

1. **Initialize Program**
   - Display a welcome message.
   - Initialize necessary variables.

2. **Process Numeric Data**
   - Read numeric data from input.
   - Perform calculations or manipulations on the numeric data.
   - Ensure that the decimal point is handled as a comma.

3. **Output Results**
   - Format the numeric data for output.
   - Display or write the formatted numeric data to an output file.

4. **End Program**
   - Display a message indicating the end of the program.
   - Terminate the program.

#### Example Implementation (Pseudo Code)

```plaintext
START PROGRAM
  DISPLAY "Welcome to the Decimal Point Handling Program"

  PERFORM PROCESS-NUMERIC-DATA

  DISPLAY "Processing Completed"
  DISPLAY "End of Program"
END PROGRAM

PROCESS-NUMERIC-DATA SECTION.
  * Read numeric data from input
  * Perform calculations or manipulations on the numeric data
  * Ensure that the decimal point is handled as a comma
  * Format the numeric data for output
  * Display or write the formatted numeric data to an output file
END SECTION.
```

#### Fields and Variables

- **Numeric Data**
  - Variables to store numeric data with a comma as the decimal point.
  - Variables to store the results of calculations or manipulations.

This documentation provides an overview of the program structure, the purpose of each section, and a high-level pseudo logic of how the program handles numeric data with a comma as the decimal point.

sumitchohan:  generate detailed documentation for current code file including all methods, fields, properties and summary of their implementation including pseudo logic

GitHub Copilot: ### Detailed Documentation for [`CTTWRT.cbl`](command:_github.copilot.openRelativePath?%5B%7B%22scheme%22%3A%22file%22%2C%22authority%22%3A%22%22%2C%22path%22%3A%22%2Fworkspaces%2Fcobol-apps%2FM365%20-%20Admin%20Center%20CSV%20Writer%2FCTTWRT.cbl%22%2C%22query%22%3A%22%22%2C%22fragment%22%3A%22%22%7D%5D "/workspaces/cobol-apps/M365 - Admin Center CSV Writer/CTTWRT.cbl")

#### Summary
[`CTTWRT.cbl`](command:_github.copilot.openRelativePath?%5B%7B%22scheme%22%3A%22file%22%2C%22authority%22%3A%22%22%2C%22path%22%3A%22%2Fworkspaces%2Fcobol-apps%2FM365%20-%20Admin%20Center%20CSV%20Writer%2FCTTWRT.cbl%22%2C%22query%22%3A%22%22%2C%22fragment%22%3A%22%22%7D%5D "/workspaces/cobol-apps/M365 - Admin Center CSV Writer/CTTWRT.cbl") is a COBOL program designed to write data to a CSV file. The program includes various divisions and sections that handle the identification, environment configuration, file control, and data processing.

#### Divisions and Sections

1. **Identification Division**
   - **PROGRAM-ID**: [`CTTWRT`](command:_github.copilot.openSymbolFromReferences?%5B%22CTTWRT%22%2C%5B%7B%22uri%22%3A%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2Fworkspaces%2Fcobol-apps%2FM365%20-%20Admin%20Center%20CSV%20Writer%2FCTTWRT.cbl%22%2C%22external%22%3A%22file%3A%2F%2F%2Fworkspaces%2Fcobol-apps%2FM365%2520-%2520Admin%2520Center%2520CSV%2520Writer%2FCTTWRT.cbl%22%2C%22path%22%3A%22%2Fworkspaces%2Fcobol-apps%2FM365%20-%20Admin%20Center%20CSV%20Writer%2FCTTWRT.cbl%22%2C%22scheme%22%3A%22file%22%7D%2C%22pos%22%3A%7B%22line%22%3A1%2C%22character%22%3A23%7D%7D%5D%5D "Go to definition")
     - This is the unique identifier for the program.
   - **Author**: Raphael Frei
   - **Date**: 2022
   - **Purpose**: Write data to a CSV file.

2. **Environment Division**
   - **Configuration Section**
     - **SPECIAL-NAMES**
       - [`DECIMAL-POINT IS COMMA`](command:_github.copilot.openSymbolFromReferences?%5B%22DECIMAL-POINT%20IS%20COMMA%22%2C%5B%7B%22uri%22%3A%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2Fworkspaces%2Fcobol-apps%2FM365%20-%20Admin%20Center%20CSV%20Writer%2FCTTWRT.cbl%22%2C%22external%22%3A%22file%3A%2F%2F%2Fworkspaces%2Fcobol-apps%2FM365%2520-%2520Admin%2520Center%2520CSV%2520Writer%2FCTTWRT.cbl%22%2C%22path%22%3A%22%2Fworkspaces%2Fcobol-apps%2FM365%20-%20Admin%20Center%20CSV%20Writer%2FCTTWRT.cbl%22%2C%22scheme%22%3A%22file%22%7D%2C%22pos%22%3A%7B%22line%22%3A14%2C%22character%22%3A11%7D%7D%5D%5D "Go to definition")
         - This specifies that the decimal point in numeric literals is represented by a comma instead of a period.

3. **Input-Output Section**
   - **File-Control**
     - This section will include the file control entries for the CSV file. The detailed implementation of the file structure is not provided in the provided excerpt.

4. **Data Division**
   - **File Section**
     - This section is reserved for file declarations. The detailed implementation of the file structure is not provided in the provided excerpt.

   - **Working-Storage Section**
     - This section is reserved for working storage variables. The detailed implementation of the variables is not provided in the provided excerpt.

5. **Procedure Division**
   - This section contains the logic of the program. The detailed implementation is not provided in the provided excerpt.

#### Pseudo Logic

1. **Initialize Program**
   - Display a welcome message.
   - Initialize necessary variables.

2. **Open CSV File**
   - Open the CSV file for writing.

3. **Write Data to CSV File**
   - Retrieve data to be written to the CSV file.
   - Format the data as CSV.
   - Write the formatted data to the CSV file.

4. **Close CSV File**
   - Close the CSV file after writing all data.

5. **End Program**
   - Display a message indicating the end of the program.
   - Terminate the program.

#### Example Implementation (Pseudo Code)

```plaintext
START PROGRAM
  DISPLAY "Welcome to the CSV Writer Program"

  OPEN OUTPUT CSV

  PERFORM WRITE-CSV-DATA

  CLOSE CSV

  DISPLAY "CSV Writing Completed"
  DISPLAY "End of Program"
END PROGRAM

WRITE-CSV-DATA SECTION.
  * Retrieve data to be written to the CSV file
  * Format the data as CSV
  * Write the formatted data to the CSV file
END SECTION.
```

#### Fields and Variables

- **CSV File**
  - **SELECT CSV**: Specifies the file path for the CSV file.
  - **ORGANIZATION IS LINE SEQUENTIAL**: Indicates that the file is organized as a sequence of lines.

This documentation provides an overview of the program structure, the purpose of each section, and a high-level pseudo logic of how the CSV writing process might be implemented.

sumitchohan:  generate detailed documentation for current code file including all methods, fields, properties and summary of their implementation including pseudo logic

GitHub Copilot: ### Detailed Documentation for [`STDGRAV.CBL`](command:_github.copilot.openRelativePath?%5B%7B%22scheme%22%3A%22file%22%2C%22authority%22%3A%22%22%2C%22path%22%3A%22%2Fworkspaces%2Fcobol-apps%2FStudent%20Grade%20Average%2FSTDGRAV.CBL%22%2C%22query%22%3A%22%22%2C%22fragment%22%3A%22%22%7D%5D "/workspaces/cobol-apps/Student Grade Average/STDGRAV.CBL")

#### Summary
[`STDGRAV.CBL`](command:_github.copilot.openRelativePath?%5B%7B%22scheme%22%3A%22file%22%2C%22authority%22%3A%22%22%2C%22path%22%3A%22%2Fworkspaces%2Fcobol-apps%2FStudent%20Grade%20Average%2FSTDGRAV.CBL%22%2C%22query%22%3A%22%22%2C%22fragment%22%3A%22%22%7D%5D "/workspaces/cobol-apps/Student Grade Average/STDGRAV.CBL") is a COBOL program designed to calculate and manage student grade averages. The program includes various divisions and sections that handle the identification, data storage, and processing of student grades.

#### Divisions and Sections

1. **Identification Division**
   - **PROGRAM-ID**: [`STDGRAV`](command:_github.copilot.openSymbolFromReferences?%5B%22STDGRAV%22%2C%5B%7B%22uri%22%3A%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2Fworkspaces%2Fcobol-apps%2FStudent%20Grade%20Average%2FSTDGRAV.CBL%22%2C%22external%22%3A%22file%3A%2F%2F%2Fworkspaces%2Fcobol-apps%2FStudent%2520Grade%2520Average%2FSTDGRAV.CBL%22%2C%22path%22%3A%22%2Fworkspaces%2Fcobol-apps%2FStudent%20Grade%20Average%2FSTDGRAV.CBL%22%2C%22scheme%22%3A%22file%22%7D%2C%22pos%22%3A%7B%22line%22%3A1%2C%22character%22%3A21%7D%7D%5D%5D "Go to definition")
     - This is the unique identifier for the program.
   - **Author**: Raphael Frei
   - **Date**: 2022
   - **Purpose**: Calculate and manage student grade averages.

2. **Data Division**
   - **File Section**
     - This section is reserved for file declarations. No files are declared in the provided excerpt.

   - **Working-Storage Section**
     - **Fields and Variables**
       - **WS-NOME-ALUNO**: [`PIC X(15)`](command:_github.copilot.openSymbolFromReferences?%5B%22PIC%20X(15)%22%2C%5B%7B%22uri%22%3A%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2Fworkspaces%2Fcobol-apps%2FStudent%20Grade%20Average%2FSTDGRAV.CBL%22%2C%22external%22%3A%22file%3A%2F%2F%2Fworkspaces%2Fcobol-apps%2FStudent%2520Grade%2520Average%2FSTDGRAV.CBL%22%2C%22path%22%3A%22%2Fworkspaces%2Fcobol-apps%2FStudent%20Grade%20Average%2FSTDGRAV.CBL%22%2C%22scheme%22%3A%22file%22%7D%2C%22pos%22%3A%7B%22line%22%3A13%2C%22character%22%3A34%7D%7D%5D%5D "Go to definition")
         - This field stores the name of the student.
       - **WS-I**: [`PIC 9(01)`](command:_github.copilot.openSymbolFromReferences?%5B%22PIC%209(01)%22%2C%5B%7B%22uri%22%3A%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2Fworkspaces%2Fcobol-apps%2FStudent%20Grade%20Average%2FSTDGRAV.CBL%22%2C%22external%22%3A%22file%3A%2F%2F%2Fworkspaces%2Fcobol-apps%2FStudent%2520Grade%2520Average%2FSTDGRAV.CBL%22%2C%22path%22%3A%22%2Fworkspaces%2Fcobol-apps%2FStudent%20Grade%20Average%2FSTDGRAV.CBL%22%2C%22scheme%22%3A%22file%22%7D%2C%22pos%22%3A%7B%22line%22%3A14%2C%22character%22%3A34%7D%7D%5D%5D "Go to definition")
         - This field is used as a counter or index.
       - **WS-RPT**: [`PIC X(01)`](command:_github.copilot.openSymbolFromReferences?%5B%22PIC%20X(01)%22%2C%5B%7B%22uri%22%3A%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2Fworkspaces%2Fcobol-apps%2FStudent%20Grade%20Average%2FSTDGRAV.CBL%22%2C%22external%22%3A%22file%3A%2F%2F%2Fworkspaces%2Fcobol-apps%2FStudent%2520Grade%2520Average%2FSTDGRAV.CBL%22%2C%22path%22%3A%22%2Fworkspaces%2Fcobol-apps%2FStudent%20Grade%20Average%2FSTDGRAV.CBL%22%2C%22scheme%22%3A%22file%22%7D%2C%22pos%22%3A%7B%22line%22%3A13%2C%22character%22%3A34%7D%7D%5D%5D "Go to definition")
         - This field is used to store a report indicator.
       - **WS-MATERIA**: This field is declared but not fully defined in the provided excerpt.

3. **Procedure Division**
   - This section contains the logic of the program. The detailed implementation is not provided in the provided excerpt.

#### Pseudo Logic

1. **Initialize Program**
   - Display a welcome message.
   - Initialize necessary variables.

2. **Input Student Data**
   - Prompt the user to enter the student's name.
   - Store the student's name in [`WS-NOME-ALUNO`](command:_github.copilot.openSymbolFromReferences?%5B%22WS-NOME-ALUNO%22%2C%5B%7B%22uri%22%3A%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2Fworkspaces%2Fcobol-apps%2FStudent%20Grade%20Average%2FSTDGRAV.CBL%22%2C%22external%22%3A%22file%3A%2F%2F%2Fworkspaces%2Fcobol-apps%2FStudent%2520Grade%2520Average%2FSTDGRAV.CBL%22%2C%22path%22%3A%22%2Fworkspaces%2Fcobol-apps%2FStudent%20Grade%20Average%2FSTDGRAV.CBL%22%2C%22scheme%22%3A%22file%22%7D%2C%22pos%22%3A%7B%22line%22%3A13%2C%22character%22%3A10%7D%7D%5D%5D "Go to definition").

3. **Process Grades**
   - Initialize a loop to process multiple grades.
   - For each grade, prompt the user to enter the grade.
   - Store the grade in an appropriate variable.
   - Update the grade average calculation.

4. **Generate Report**
   - Check the report indicator [`WS-RPT`](command:_github.copilot.openSymbolFromReferences?%5B%22WS-RPT%22%2C%5B%7B%22uri%22%3A%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2Fworkspaces%2Fcobol-apps%2FStudent%20Grade%20Average%2FSTDGRAV.CBL%22%2C%22external%22%3A%22file%3A%2F%2F%2Fworkspaces%2Fcobol-apps%2FStudent%2520Grade%2520Average%2FSTDGRAV.CBL%22%2C%22path%22%3A%22%2Fworkspaces%2Fcobol-apps%2FStudent%20Grade%20Average%2FSTDGRAV.CBL%22%2C%22scheme%22%3A%22file%22%7D%2C%22pos%22%3A%7B%22line%22%3A13%2C%22character%22%3A10%7D%7D%5D%5D "Go to definition").
   - If the report is required, format and display the student's grade average.

5. **End Program**
   - Display a message indicating the end of the program.
   - Terminate the program.

#### Example Implementation (Pseudo Code)

```plaintext
START PROGRAM
  DISPLAY "Welcome to the Student Grade Average Program"

  PERFORM INPUT-STUDENT-DATA
  PERFORM PROCESS-GRADES
  PERFORM GENERATE-REPORT

  DISPLAY "End of Program"
END PROGRAM

INPUT-STUDENT-DATA SECTION.
  DISPLAY "Enter Student Name: "
  ACCEPT WS-NOME-ALUNO
END SECTION.

PROCESS-GRADES SECTION.
  PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > NUMBER-OF-GRADES
    DISPLAY "Enter Grade: "
    ACCEPT WS-GRADE
    * Update grade average calculation
  END-PERFORM
END SECTION.

GENERATE-REPORT SECTION.
  IF WS-RPT = 'Y'
    * Format and display the student's grade average
  END-IF
END SECTION.
```

#### Fields and Variables

- **WS-NOME-ALUNO**: Stores the name of the student.
- **WS-I**: Used as a counter or index.
- **WS-RPT**: Stores a report indicator.
- **WS-MATERIA**: Declared but not fully defined in the provided excerpt.

This documentation provides an overview of the program structure, the purpose of each section, and a high-level pseudo logic of how the student grade average calculation process might be implemented.

sumitchohan:  generate detailed documentation for current code file including all methods, fields, properties and summary of their implementation including pseudo logic

GitHub Copilot: ### Detailed Documentation for [`PG01HLLWD.cbl`](command:_github.copilot.openRelativePath?%5B%7B%22scheme%22%3A%22file%22%2C%22authority%22%3A%22%22%2C%22path%22%3A%22%2Fworkspaces%2Fcobol-apps%2FTraining%20Apps%2FPG01HLLWD.cbl%22%2C%22query%22%3A%22%22%2C%22fragment%22%3A%22%22%7D%5D "/workspaces/cobol-apps/Training Apps/PG01HLLWD.cbl")

#### Summary
[`PG01HLLWD.cbl`](command:_github.copilot.openRelativePath?%5B%7B%22scheme%22%3A%22file%22%2C%22authority%22%3A%22%22%2C%22path%22%3A%22%2Fworkspaces%2Fcobol-apps%2FTraining%20Apps%2FPG01HLLWD.cbl%22%2C%22query%22%3A%22%22%2C%22fragment%22%3A%22%22%7D%5D "/workspaces/cobol-apps/Training Apps/PG01HLLWD.cbl") is a simple COBOL program designed to display the message "Hello World!" on the screen. This program serves as an introductory example for learning COBOL programming.

#### Divisions and Sections

1. **Identification Division**
   - **PROGRAM-ID**: [`PG01HLLWD`](command:_github.copilot.openSymbolFromReferences?%5B%22PG01HLLWD%22%2C%5B%7B%22uri%22%3A%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2Fworkspaces%2Fcobol-apps%2FTraining%20Apps%2FPG01HLLWD.cbl%22%2C%22external%22%3A%22file%3A%2F%2F%2Fworkspaces%2Fcobol-apps%2FTraining%2520Apps%2FPG01HLLWD.cbl%22%2C%22path%22%3A%22%2Fworkspaces%2Fcobol-apps%2FTraining%20Apps%2FPG01HLLWD.cbl%22%2C%22scheme%22%3A%22file%22%7D%2C%22pos%22%3A%7B%22line%22%3A6%2C%22character%22%3A19%7D%7D%5D%5D "Go to definition")
     - This is the unique identifier for the program.
   - **Author**: Raphael Frei
   - **Date**: 15/06/2022
   - **Purpose**: First Program

2. **Data Division**
   - **File Section**
     - This section is reserved for file declarations. No files are declared in this program.
   - **Working-Storage Section**
     - This section is reserved for working storage variables. No variables are declared in this program.

3. **Procedure Division**
   - **MAIN-PROCEDURE**
     - This section contains the main logic of the program.
     - **DISPLAY "Hello World!"**
       - This statement outputs the text "Hello World!" to the screen.
     - **STOP RUN**
       - This statement terminates the program.

#### Pseudo Logic

1. **Initialize Program**
   - The program starts execution.

2. **Display Message**
   - The program displays the message "Hello World!" on the screen.

3. **Terminate Program**
   - The program terminates execution.

#### Example Implementation (Pseudo Code)

```plaintext
START PROGRAM
  DISPLAY "Hello World!"
  STOP RUN
END PROGRAM
```

#### Fields and Variables

- **None**
  - This program does not declare any fields or variables.

This documentation provides an overview of the program structure, the purpose of each section, and a high-level pseudo logic of how the "Hello World!" message is displayed. The program is straightforward and serves as a basic example for beginners in COBOL programming.

sumitchohan:  generate detailed documentation for current code file including all methods, fields, properties and summary of their implementation including pseudo logic

GitHub Copilot: ### Detailed Documentation for [`PG04ISTDT.cbl`](command:_github.copilot.openRelativePath?%5B%7B%22scheme%22%3A%22file%22%2C%22authority%22%3A%22%22%2C%22path%22%3A%22%2Fworkspaces%2Fcobol-apps%2FTraining%20Apps%2FPG04ISTDT.cbl%22%2C%22query%22%3A%22%22%2C%22fragment%22%3A%22%22%7D%5D "/workspaces/cobol-apps/Training Apps/PG04ISTDT.cbl")

#### Summary
[`PG04ISTDT.cbl`](command:_github.copilot.openRelativePath?%5B%7B%22scheme%22%3A%22file%22%2C%22authority%22%3A%22%22%2C%22path%22%3A%22%2Fworkspaces%2Fcobol-apps%2FTraining%20Apps%2FPG04ISTDT.cbl%22%2C%22query%22%3A%22%22%2C%22fragment%22%3A%22%22%7D%5D "/workspaces/cobol-apps/Training Apps/PG04ISTDT.cbl") is a COBOL program designed to display a prompt for user input, accept the input, and then display the entered value. This program serves as an example of basic input and output operations in COBOL.

#### Divisions and Sections

1. **Identification Division**
   - **PROGRAM-ID**: [`YOUR-PROGRAM-NAME`](command:_github.copilot.openSymbolFromReferences?%5B%22YOUR-PROGRAM-NAME%22%2C%5B%7B%22uri%22%3A%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2Fworkspaces%2Fcobol-apps%2FTraining%20Apps%2FPG04ISTDT.cbl%22%2C%22external%22%3A%22file%3A%2F%2F%2Fworkspaces%2Fcobol-apps%2FTraining%2520Apps%2FPG04ISTDT.cbl%22%2C%22path%22%3A%22%2Fworkspaces%2Fcobol-apps%2FTraining%20Apps%2FPG04ISTDT.cbl%22%2C%22scheme%22%3A%22file%22%7D%2C%22pos%22%3A%7B%22line%22%3A6%2C%22character%22%3A19%7D%7D%5D%5D "Go to definition")
     - This is the unique identifier for the program.
   - **Author**: Raphael Frei
   - **Date**: 16/06/2022
   - **Purpose**: Display user input

2. **Data Division**
   - **File Section**
     - This section is reserved for file declarations. No files are declared in this program.
   - **Working-Storage Section**
     - **Fields and Variables**
       - **WS-DATA**: [`PIC X(10) VALUE SPACES`](command:_github.copilot.openSymbolFromReferences?%5B%22PIC%20X(10)%20VALUE%20SPACES%22%2C%5B%7B%22uri%22%3A%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2Fworkspaces%2Fcobol-apps%2FTraining%20Apps%2FPG04ISTDT.cbl%22%2C%22external%22%3A%22file%3A%2F%2F%2Fworkspaces%2Fcobol-apps%2FTraining%2520Apps%2FPG04ISTDT.cbl%22%2C%22path%22%3A%22%2Fworkspaces%2Fcobol-apps%2FTraining%20Apps%2FPG04ISTDT.cbl%22%2C%22scheme%22%3A%22file%22%7D%2C%22pos%22%3A%7B%22line%22%3A10%2C%22character%22%3A27%7D%7D%5D%5D "Go to definition")
         - This field is used to store the user input. It is defined as a 10-character alphanumeric field initialized with spaces.

3. **Procedure Division**
   - **MAIN-PROCEDURE**
     - This section contains the main logic of the program.
     - **DISPLAY "INSIRA UM VALOR: "**
       - This statement outputs the prompt "INSIRA UM VALOR: " to the screen.
     - **ACCEPT WS-DATA**
       - This statement accepts user input and stores it in the [`WS-DATA`](command:_github.copilot.openSymbolFromReferences?%5B%22WS-DATA%22%2C%5B%7B%22uri%22%3A%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2Fworkspaces%2Fcobol-apps%2FTraining%20Apps%2FPG04ISTDT.cbl%22%2C%22external%22%3A%22file%3A%2F%2F%2Fworkspaces%2Fcobol-apps%2FTraining%2520Apps%2FPG04ISTDT.cbl%22%2C%22path%22%3A%22%2Fworkspaces%2Fcobol-apps%2FTraining%20Apps%2FPG04ISTDT.cbl%22%2C%22scheme%22%3A%22file%22%7D%2C%22pos%22%3A%7B%22line%22%3A10%2C%22character%22%3A10%7D%7D%5D%5D "Go to definition") field.
     - **DISPLAY " "**
       - This statement outputs a blank line to the screen.

#### Pseudo Logic

1. **Initialize Program**
   - The program starts execution.

2. **Display Prompt**
   - The program displays the prompt "INSIRA UM VALOR: " on the screen.

3. **Accept User Input**
   - The program accepts user input and stores it in the [`WS-DATA`](command:_github.copilot.openSymbolFromReferences?%5B%22WS-DATA%22%2C%5B%7B%22uri%22%3A%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2Fworkspaces%2Fcobol-apps%2FTraining%20Apps%2FPG04ISTDT.cbl%22%2C%22external%22%3A%22file%3A%2F%2F%2Fworkspaces%2Fcobol-apps%2FTraining%2520Apps%2FPG04ISTDT.cbl%22%2C%22path%22%3A%22%2Fworkspaces%2Fcobol-apps%2FTraining%20Apps%2FPG04ISTDT.cbl%22%2C%22scheme%22%3A%22file%22%7D%2C%22pos%22%3A%7B%22line%22%3A10%2C%22character%22%3A10%7D%7D%5D%5D "Go to definition") field.

4. **Display Blank Line**
   - The program displays a blank line on the screen.

5. **Terminate Program**
   - The program terminates execution.

#### Example Implementation (Pseudo Code)

```plaintext
START PROGRAM
  DISPLAY "INSIRA UM VALOR: "
  ACCEPT WS-DATA
  DISPLAY " "
  STOP RUN
END PROGRAM
```

#### Fields and Variables

- **WS-DATA**: [`PIC X(10) VALUE SPACES`](command:_github.copilot.openSymbolFromReferences?%5B%22PIC%20X(10)%20VALUE%20SPACES%22%2C%5B%7B%22uri%22%3A%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2Fworkspaces%2Fcobol-apps%2FTraining%20Apps%2FPG04ISTDT.cbl%22%2C%22external%22%3A%22file%3A%2F%2F%2Fworkspaces%2Fcobol-apps%2FTraining%2520Apps%2FPG04ISTDT.cbl%22%2C%22path%22%3A%22%2Fworkspaces%2Fcobol-apps%2FTraining%20Apps%2FPG04ISTDT.cbl%22%2C%22scheme%22%3A%22file%22%7D%2C%22pos%22%3A%7B%22line%22%3A10%2C%22character%22%3A27%7D%7D%5D%5D "Go to definition")
  - This field is used to store the user input. It is defined as a 10-character alphanumeric field initialized with spaces.

This documentation provides an overview of the program structure, the purpose of each section, and a high-level pseudo logic of how the user input is handled and displayed. The program is straightforward and serves as a basic example of input and output operations in COBOL.

sumitchohan:  generate detailed documentation for current code file including all methods, fields, properties and summary of their implementation including pseudo logic

GitHub Copilot: ### Detailed Documentation for [`PG06CALC.cbl`](command:_github.copilot.openRelativePath?%5B%7B%22scheme%22%3A%22file%22%2C%22authority%22%3A%22%22%2C%22path%22%3A%22%2Fworkspaces%2Fcobol-apps%2FTraining%20Apps%2FPG06CALC.cbl%22%2C%22query%22%3A%22%22%2C%22fragment%22%3A%22%22%7D%5D "/workspaces/cobol-apps/Training Apps/PG06CALC.cbl")

#### Summary
[`PG06CALC.cbl`](command:_github.copilot.openRelativePath?%5B%7B%22scheme%22%3A%22file%22%2C%22authority%22%3A%22%22%2C%22path%22%3A%22%2Fworkspaces%2Fcobol-apps%2FTraining%20Apps%2FPG06CALC.cbl%22%2C%22query%22%3A%22%22%2C%22fragment%22%3A%22%22%7D%5D "/workspaces/cobol-apps/Training Apps/PG06CALC.cbl") is a COBOL program designed to perform basic arithmetic operations on two user-provided numbers. The program prompts the user to input two numbers, performs a calculation, and displays the result. This program serves as an example of basic input, processing, and output operations in COBOL.

#### Divisions and Sections

1. **Identification Division**
   - **PROGRAM-ID**: [`PG06CALC`](command:_github.copilot.openSymbolFromReferences?%5B%22PG06CALC%22%2C%5B%7B%22uri%22%3A%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2Fworkspaces%2Fcobol-apps%2FTraining%20Apps%2FPG06CALC.cbl%22%2C%22external%22%3A%22file%3A%2F%2F%2Fworkspaces%2Fcobol-apps%2FTraining%2520Apps%2FPG06CALC.cbl%22%2C%22path%22%3A%22%2Fworkspaces%2Fcobol-apps%2FTraining%20Apps%2FPG06CALC.cbl%22%2C%22scheme%22%3A%22file%22%7D%2C%22pos%22%3A%7B%22line%22%3A6%2C%22character%22%3A19%7D%7D%5D%5D "Go to definition")
     - This is the unique identifier for the program.
   - **Author**: Raphael Frei
   - **Date**: 28/06/2022
   - **Purpose**: Perform basic arithmetic operations on two user-provided numbers.

2. **Data Division**
   - **File Section**
     - This section is reserved for file declarations. No files are declared in this program.
   - **Working-Storage Section**
     - **Fields and Variables**
       - **WS-N1**: [`PIC 9(03) VALUE ZEROS`](command:_github.copilot.openSymbolFromReferences?%5B%22PIC%209(03)%20VALUE%20ZEROS%22%2C%5B%7B%22uri%22%3A%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2Fworkspaces%2Fcobol-apps%2FTraining%20Apps%2FPG06CALC.cbl%22%2C%22external%22%3A%22file%3A%2F%2F%2Fworkspaces%2Fcobol-apps%2FTraining%2520Apps%2FPG06CALC.cbl%22%2C%22path%22%3A%22%2Fworkspaces%2Fcobol-apps%2FTraining%20Apps%2FPG06CALC.cbl%22%2C%22scheme%22%3A%22file%22%7D%2C%22pos%22%3A%7B%22line%22%3A10%2C%22character%22%3A27%7D%7D%5D%5D "Go to definition")
         - This field is used to store the first user-provided number. It is defined as a 3-digit numeric field initialized to zeros.
       - **WS-N2**: [`PIC 9(03) VALUE ZEROS`](command:_github.copilot.openSymbolFromReferences?%5B%22PIC%209(03)%20VALUE%20ZEROS%22%2C%5B%7B%22uri%22%3A%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2Fworkspaces%2Fcobol-apps%2FTraining%20Apps%2FPG06CALC.cbl%22%2C%22external%22%3A%22file%3A%2F%2F%2Fworkspaces%2Fcobol-apps%2FTraining%2520Apps%2FPG06CALC.cbl%22%2C%22path%22%3A%22%2Fworkspaces%2Fcobol-apps%2FTraining%20Apps%2FPG06CALC.cbl%22%2C%22scheme%22%3A%22file%22%7D%2C%22pos%22%3A%7B%22line%22%3A10%2C%22character%22%3A27%7D%7D%5D%5D "Go to definition")
         - This field is used to store the second user-provided number. It is defined as a 3-digit numeric field initialized to zeros.
       - **WS-RS**: [`PIC 9(07) VALUE ZEROS`](command:_github.copilot.openSymbolFromReferences?%5B%22PIC%209(07)%20VALUE%20ZEROS%22%2C%5B%7B%22uri%22%3A%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2Fworkspaces%2Fcobol-apps%2FTraining%20Apps%2FPG06CALC.cbl%22%2C%22external%22%3A%22file%3A%2F%2F%2Fworkspaces%2Fcobol-apps%2FTraining%2520Apps%2FPG06CALC.cbl%22%2C%22path%22%3A%22%2Fworkspaces%2Fcobol-apps%2FTraining%20Apps%2FPG06CALC.cbl%22%2C%22scheme%22%3A%22file%22%7D%2C%22pos%22%3A%7B%22line%22%3A10%2C%22character%22%3A27%7D%7D%5D%5D "Go to definition")
         - This field is used to store the result of the arithmetic operation. It is defined as a 7-digit numeric field initialized to zeros.

3. **Procedure Division**
   - **MAIN-PROCEDURE**
     - This section contains the main logic of the program.
     - **DISPLAY "INSIRA O PRIMEIRO NUMERO: "**
       - This statement outputs the prompt "INSIRA O PRIMEIRO NUMERO: " to the screen.
     - **ACCEPT WS-N1**
       - This statement accepts user input and stores it in the [`WS-N1`](command:_github.copilot.openSymbolFromReferences?%5B%22WS-N1%22%2C%5B%7B%22uri%22%3A%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2Fworkspaces%2Fcobol-apps%2FTraining%20Apps%2FPG06CALC.cbl%22%2C%22external%22%3A%22file%3A%2F%2F%2Fworkspaces%2Fcobol-apps%2FTraining%2520Apps%2FPG06CALC.cbl%22%2C%22path%22%3A%22%2Fworkspaces%2Fcobol-apps%2FTraining%20Apps%2FPG06CALC.cbl%22%2C%22scheme%22%3A%22file%22%7D%2C%22pos%22%3A%7B%22line%22%3A10%2C%22character%22%3A10%7D%7D%5D%5D "Go to definition") field.
     - **DISPLAY "INSIRA O SEGUNDO NUMERO: "**
       - This statement outputs the prompt "INSIRA O SEGUNDO NUMERO: " to the screen.
     - **ACCEPT WS-N2**
       - This statement accepts user input and stores it in the [`WS-N2`](command:_github.copilot.openSymbolFromReferences?%5B%22WS-N2%22%2C%5B%7B%22uri%22%3A%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2Fworkspaces%2Fcobol-apps%2FTraining%20Apps%2FPG06CALC.cbl%22%2C%22external%22%3A%22file%3A%2F%2F%2Fworkspaces%2Fcobol-apps%2FTraining%2520Apps%2FPG06CALC.cbl%22%2C%22path%22%3A%22%2Fworkspaces%2Fcobol-apps%2FTraining%20Apps%2FPG06CALC.cbl%22%2C%22scheme%22%3A%22file%22%7D%2C%22pos%22%3A%7B%22line%22%3A10%2C%22character%22%3A10%7D%7D%5D%5D "Go to definition") field.
     - **COMPUTE WS-RS = WS-N1 + WS-N2**
       - This statement performs the addition of [`WS-N1`](command:_github.copilot.openSymbolFromReferences?%5B%22WS-N1%22%2C%5B%7B%22uri%22%3A%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2Fworkspaces%2Fcobol-apps%2FTraining%20Apps%2FPG06CALC.cbl%22%2C%22external%22%3A%22file%3A%2F%2F%2Fworkspaces%2Fcobol-apps%2FTraining%2520Apps%2FPG06CALC.cbl%22%2C%22path%22%3A%22%2Fworkspaces%2Fcobol-apps%2FTraining%20Apps%2FPG06CALC.cbl%22%2C%22scheme%22%3A%22file%22%7D%2C%22pos%22%3A%7B%22line%22%3A10%2C%22character%22%3A10%7D%7D%5D%5D "Go to definition") and [`WS-N2`](command:_github.copilot.openSymbolFromReferences?%5B%22WS-N2%22%2C%5B%7B%22uri%22%3A%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2Fworkspaces%2Fcobol-apps%2FTraining%20Apps%2FPG06CALC.cbl%22%2C%22external%22%3A%22file%3A%2F%2F%2Fworkspaces%2Fcobol-apps%2FTraining%2520Apps%2FPG06CALC.cbl%22%2C%22path%22%3A%22%2Fworkspaces%2Fcobol-apps%2FTraining%20Apps%2FPG06CALC.cbl%22%2C%22scheme%22%3A%22file%22%7D%2C%22pos%22%3A%7B%22line%22%3A10%2C%22character%22%3A10%7D%7D%5D%5D "Go to definition") and stores the result in [`WS-RS`](command:_github.copilot.openSymbolFromReferences?%5B%22WS-RS%22%2C%5B%7B%22uri%22%3A%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2Fworkspaces%2Fcobol-apps%2FTraining%20Apps%2FPG06CALC.cbl%22%2C%22external%22%3A%22file%3A%2F%2F%2Fworkspaces%2Fcobol-apps%2FTraining%2520Apps%2FPG06CALC.cbl%22%2C%22path%22%3A%22%2Fworkspaces%2Fcobol-apps%2FTraining%20Apps%2FPG06CALC.cbl%22%2C%22scheme%22%3A%22file%22%7D%2C%22pos%22%3A%7B%22line%22%3A10%2C%22character%22%3A10%7D%7D%5D%5D "Go to definition").
     - **DISPLAY "RESULTADO: " WS-RS**
       - This statement outputs the result stored in [`WS-RS`](command:_github.copilot.openSymbolFromReferences?%5B%22WS-RS%22%2C%5B%7B%22uri%22%3A%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2Fworkspaces%2Fcobol-apps%2FTraining%20Apps%2FPG06CALC.cbl%22%2C%22external%22%3A%22file%3A%2F%2F%2Fworkspaces%2Fcobol-apps%2FTraining%2520Apps%2FPG06CALC.cbl%22%2C%22path%22%3A%22%2Fworkspaces%2Fcobol-apps%2FTraining%20Apps%2FPG06CALC.cbl%22%2C%22scheme%22%3A%22file%22%7D%2C%22pos%22%3A%7B%22line%22%3A10%2C%22character%22%3A10%7D%7D%5D%5D "Go to definition") to the screen.
     - **STOP RUN**
       - This statement terminates the program.

#### Pseudo Logic

1. **Initialize Program**
   - The program starts execution.

2. **Display Prompt for First Number**
   - The program displays the prompt "INSIRA O PRIMEIRO NUMERO: " on the screen.

3. **Accept First Number**
   - The program accepts user input and stores it in the [`WS-N1`](command:_github.copilot.openSymbolFromReferences?%5B%22WS-N1%22%2C%5B%7B%22uri%22%3A%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2Fworkspaces%2Fcobol-apps%2FTraining%20Apps%2FPG06CALC.cbl%22%2C%22external%22%3A%22file%3A%2F%2F%2Fworkspaces%2Fcobol-apps%2FTraining%2520Apps%2FPG06CALC.cbl%22%2C%22path%22%3A%22%2Fworkspaces%2Fcobol-apps%2FTraining%20Apps%2FPG06CALC.cbl%22%2C%22scheme%22%3A%22file%22%7D%2C%22pos%22%3A%7B%22line%22%3A10%2C%22character%22%3A10%7D%7D%5D%5D "Go to definition") field.

4. **Display Prompt for Second Number**
   - The program displays the prompt "INSIRA O SEGUNDO NUMERO: " on the screen.

5. **Accept Second Number**
   - The program accepts user input and stores it in the [`WS-N2`](command:_github.copilot.openSymbolFromReferences?%5B%22WS-N2%22%2C%5B%7B%22uri%22%3A%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2Fworkspaces%2Fcobol-apps%2FTraining%20Apps%2FPG06CALC.cbl%22%2C%22external%22%3A%22file%3A%2F%2F%2Fworkspaces%2Fcobol-apps%2FTraining%2520Apps%2FPG06CALC.cbl%22%2C%22path%22%3A%22%2Fworkspaces%2Fcobol-apps%2FTraining%20Apps%2FPG06CALC.cbl%22%2C%22scheme%22%3A%22file%22%7D%2C%22pos%22%3A%7B%22line%22%3A10%2C%22character%22%3A10%7D%7D%5D%5D "Go to definition") field.

6. **Perform Addition**
   - The program adds the values of [`WS-N1`](command:_github.copilot.openSymbolFromReferences?%5B%22WS-N1%22%2C%5B%7B%22uri%22%3A%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2Fworkspaces%2Fcobol-apps%2FTraining%20Apps%2FPG06CALC.cbl%22%2C%22external%22%3A%22file%3A%2F%2F%2Fworkspaces%2Fcobol-apps%2FTraining%2520Apps%2FPG06CALC.cbl%22%2C%22path%22%3A%22%2Fworkspaces%2Fcobol-apps%2FTraining%20Apps%2FPG06CALC.cbl%22%2C%22scheme%22%3A%22file%22%7D%2C%22pos%22%3A%7B%22line%22%3A10%2C%22character%22%3A10%7D%7D%5D%5D "Go to definition") and [`WS-N2`](command:_github.copilot.openSymbolFromReferences?%5B%22WS-N2%22%2C%5B%7B%22uri%22%3A%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2Fworkspaces%2Fcobol-apps%2FTraining%20Apps%2FPG06CALC.cbl%22%2C%22external%22%3A%22file%3A%2F%2F%2Fworkspaces%2Fcobol-apps%2FTraining%2520Apps%2FPG06CALC.cbl%22%2C%22path%22%3A%22%2Fworkspaces%2Fcobol-apps%2FTraining%20Apps%2FPG06CALC.cbl%22%2C%22scheme%22%3A%22file%22%7D%2C%22pos%22%3A%7B%22line%22%3A10%2C%22character%22%3A10%7D%7D%5D%5D "Go to definition") and stores the result in [`WS-RS`](command:_github.copilot.openSymbolFromReferences?%5B%22WS-RS%22%2C%5B%7B%22uri%22%3A%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2Fworkspaces%2Fcobol-apps%2FTraining%20Apps%2FPG06CALC.cbl%22%2C%22external%22%3A%22file%3A%2F%2F%2Fworkspaces%2Fcobol-apps%2FTraining%2520Apps%2FPG06CALC.cbl%22%2C%22path%22%3A%22%2Fworkspaces%2Fcobol-apps%2FTraining%20Apps%2FPG06CALC.cbl%22%2C%22scheme%22%3A%22file%22%7D%2C%22pos%22%3A%7B%22line%22%3A10%2C%22character%22%3A10%7D%7D%5D%5D "Go to definition").

7. **Display Result**
   - The program displays the result stored in [`WS-RS`](command:_github.copilot.openSymbolFromReferences?%5B%22WS-RS%22%2C%5B%7B%22uri%22%3A%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2Fworkspaces%2Fcobol-apps%2FTraining%20Apps%2FPG06CALC.cbl%22%2C%22external%22%3A%22file%3A%2F%2F%2Fworkspaces%2Fcobol-apps%2FTraining%2520Apps%2FPG06CALC.cbl%22%2C%22path%22%3A%22%2Fworkspaces%2Fcobol-apps%2FTraining%20Apps%2FPG06CALC.cbl%22%2C%22scheme%22%3A%22file%22%7D%2C%22pos%22%3A%7B%22line%22%3A10%2C%22character%22%3A10%7D%7D%5D%5D "Go to definition") on the screen.

8. **Terminate Program**
   - The program terminates execution.

#### Example Implementation (Pseudo Code)

```plaintext
START PROGRAM
  DISPLAY "INSIRA O PRIMEIRO NUMERO: "
  ACCEPT WS-N1
  DISPLAY "INSIRA O SEGUNDO NUMERO: "
  ACCEPT WS-N2
  COMPUTE WS-RS = WS-N1 + WS-N2
  DISPLAY "RESULTADO: " WS-RS
  STOP RUN
END PROGRAM
```

#### Fields and Variables

- **WS-N1**: [`PIC 9(03) VALUE ZEROS`](command:_github.copilot.openSymbolFromReferences?%5B%22PIC%209(03)%20VALUE%20ZEROS%22%2C%5B%7B%22uri%22%3A%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2Fworkspaces%2Fcobol-apps%2FTraining%20Apps%2FPG06CALC.cbl%22%2C%22external%22%3A%22file%3A%2F%2F%2Fworkspaces%2Fcobol-apps%2FTraining%2520Apps%2FPG06CALC.cbl%22%2C%22path%22%3A%22%2Fworkspaces%2Fcobol-apps%2FTraining%20Apps%2FPG06CALC.cbl%22%2C%22scheme%22%3A%22file%22%7D%2C%22pos%22%3A%7B%22line%22%3A10%2C%22character%22%3A27%7D%7D%5D%5D "Go to definition")
  - This field is used to store the first user-provided number. It is defined as a 3-digit numeric field initialized to zeros.
- **WS-N2**: [`PIC 9(03) VALUE ZEROS`](command:_github.copilot.openSymbolFromReferences?%5B%22PIC%209(03)%20VALUE%20ZEROS%22%2C%5B%7B%22uri%22%3A%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2Fworkspaces%2Fcobol-apps%2FTraining%20Apps%2FPG06CALC.cbl%22%2C%22external%22%3A%22file%3A%2F%2F%2Fworkspaces%2Fcobol-apps%2FTraining%2520Apps%2FPG06CALC.cbl%22%2C%22path%22%3A%22%2Fworkspaces%2Fcobol-apps%2FTraining%20Apps%2FPG06CALC.cbl%22%2C%22scheme%22%3A%22file%22%7D%2C%22pos%22%3A%7B%22line%22%3A10%2C%22character%22%3A27%7D%7D%5D%5D "Go to definition")
  - This field is used to store the second user-provided number. It is defined as a 3-digit numeric field initialized to zeros.
- **WS-RS**: [`PIC 9(07) VALUE ZEROS`](command:_github.copilot.openSymbolFromReferences?%5B%22PIC%209(07)%20VALUE%20ZEROS%22%2C%5B%7B%22uri%22%3A%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2Fworkspaces%2Fcobol-apps%2FTraining%20Apps%2FPG06CALC.cbl%22%2C%22external%22%3A%22file%3A%2F%2F%2Fworkspaces%2Fcobol-apps%2FTraining%2520Apps%2FPG06CALC.cbl%22%2C%22path%22%3A%22%2Fworkspaces%2Fcobol-apps%2FTraining%20Apps%2FPG06CALC.cbl%22%2C%22scheme%22%3A%22file%22%7D%2C%22pos%22%3A%7B%22line%22%3A10%2C%22character%22%3A27%7D%7D%5D%5D "Go to definition")
  - This field is used to store the result of the arithmetic operation. It is defined as a 7-digit numeric field initialized to zeros.

This documentation provides an overview of the program structure, the purpose of each section, and a high-level pseudo logic of how the arithmetic operation is performed and displayed. The program is straightforward and serves as a basic example of input, processing, and output operations in COBOL.

sumitchohan:  generate detailed documentation for current code file including all methods, fields, properties and summary of their implementation including pseudo logic

GitHub Copilot: ### Detailed Documentation for [`PG08MEDIA.cbl`](command:_github.copilot.openRelativePath?%5B%7B%22scheme%22%3A%22file%22%2C%22authority%22%3A%22%22%2C%22path%22%3A%22%2Fworkspaces%2Fcobol-apps%2FTraining%20Apps%2FPG08MEDIA.cbl%22%2C%22query%22%3A%22%22%2C%22fragment%22%3A%22%22%7D%5D "/workspaces/cobol-apps/Training Apps/PG08MEDIA.cbl")

#### Summary
[`PG08MEDIA.cbl`](command:_github.copilot.openRelativePath?%5B%7B%22scheme%22%3A%22file%22%2C%22authority%22%3A%22%22%2C%22path%22%3A%22%2Fworkspaces%2Fcobol-apps%2FTraining%20Apps%2FPG08MEDIA.cbl%22%2C%22query%22%3A%22%22%2C%22fragment%22%3A%22%22%7D%5D "/workspaces/cobol-apps/Training Apps/PG08MEDIA.cbl") is a COBOL program designed to calculate the average grade for a student in a specific subject. The program stores the student's name, iterates through multiple subjects, and calculates the average grade for each subject. This program demonstrates basic data handling, iteration, and arithmetic operations in COBOL.

#### Divisions and Sections

1. **Identification Division**
   - **PROGRAM-ID**: [`PG08MEDIA`](command:_github.copilot.openSymbolFromReferences?%5B%22PG08MEDIA%22%2C%5B%7B%22uri%22%3A%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2Fworkspaces%2Fcobol-apps%2FTraining%20Apps%2FPG08MEDIA.cbl%22%2C%22external%22%3A%22file%3A%2F%2F%2Fworkspaces%2Fcobol-apps%2FTraining%2520Apps%2FPG08MEDIA.cbl%22%2C%22path%22%3A%22%2Fworkspaces%2Fcobol-apps%2FTraining%20Apps%2FPG08MEDIA.cbl%22%2C%22scheme%22%3A%22file%22%7D%2C%22pos%22%3A%7B%22line%22%3A6%2C%22character%22%3A19%7D%7D%5D%5D "Go to definition")
     - This is the unique identifier for the program.
   - **Author**: Raphael Frei
   - **Date**: 19/06/2022
   - **Purpose**: Calculate the average grade for a student in a specific subject.

2. **Data Division**
   - **File Section**
     - This section is reserved for file declarations. No files are declared in this program.
   - **Working-Storage Section**
     - **Fields and Variables**
       - **WS-NOME-ALUNO**: [`PIC X(15)`](command:_github.copilot.openSymbolFromReferences?%5B%22PIC%20X(15)%22%2C%5B%7B%22uri%22%3A%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2Fworkspaces%2Fcobol-apps%2FTraining%20Apps%2FPG08MEDIA.cbl%22%2C%22external%22%3A%22file%3A%2F%2F%2Fworkspaces%2Fcobol-apps%2FTraining%2520Apps%2FPG08MEDIA.cbl%22%2C%22path%22%3A%22%2Fworkspaces%2Fcobol-apps%2FTraining%20Apps%2FPG08MEDIA.cbl%22%2C%22scheme%22%3A%22file%22%7D%2C%22pos%22%3A%7B%22line%22%3A11%2C%22character%22%3A34%7D%7D%5D%5D "Go to definition")
         - This field is used to store the student's name. It is defined as a 15-character alphanumeric field.
       - **WS-I**: [`PIC 9(01)`](command:_github.copilot.openSymbolFromReferences?%5B%22PIC%209(01)%22%2C%5B%7B%22uri%22%3A%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2Fworkspaces%2Fcobol-apps%2FTraining%20Apps%2FPG08MEDIA.cbl%22%2C%22external%22%3A%22file%3A%2F%2F%2Fworkspaces%2Fcobol-apps%2FTraining%2520Apps%2FPG08MEDIA.cbl%22%2C%22path%22%3A%22%2Fworkspaces%2Fcobol-apps%2FTraining%20Apps%2FPG08MEDIA.cbl%22%2C%22scheme%22%3A%22file%22%7D%2C%22pos%22%3A%7B%22line%22%3A12%2C%22character%22%3A34%7D%7D%5D%5D "Go to definition")
         - This field is used as a counter for iteration. It is defined as a 1-digit numeric field.
       - **WS-RPT**: [`PIC X(01)`](command:_github.copilot.openSymbolFromReferences?%5B%22PIC%20X(01)%22%2C%5B%7B%22uri%22%3A%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2Fworkspaces%2Fcobol-apps%2FTraining%20Apps%2FPG08MEDIA.cbl%22%2C%22external%22%3A%22file%3A%2F%2F%2Fworkspaces%2Fcobol-apps%2FTraining%2520Apps%2FPG08MEDIA.cbl%22%2C%22path%22%3A%22%2Fworkspaces%2Fcobol-apps%2FTraining%20Apps%2FPG08MEDIA.cbl%22%2C%22scheme%22%3A%22file%22%7D%2C%22pos%22%3A%7B%22line%22%3A11%2C%22character%22%3A34%7D%7D%5D%5D "Go to definition")
         - This field is used to store a single character, possibly for a repeat prompt. It is defined as a 1-character alphanumeric field.
       - **WS-MATERIA**
         - **WS-NOME-MATERIA**: [`PIC X(10)`](command:_github.copilot.openSymbolFromReferences?%5B%22PIC%20X(10)%22%2C%5B%7B%22uri%22%3A%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2Fworkspaces%2Fcobol-apps%2FTraining%20Apps%2FPG08MEDIA.cbl%22%2C%22external%22%3A%22file%3A%2F%2F%2Fworkspaces%2Fcobol-apps%2FTraining%2520Apps%2FPG08MEDIA.cbl%22%2C%22path%22%3A%22%2Fworkspaces%2Fcobol-apps%2FTraining%20Apps%2FPG08MEDIA.cbl%22%2C%22scheme%22%3A%22file%22%7D%2C%22pos%22%3A%7B%22line%22%3A11%2C%22character%22%3A34%7D%7D%5D%5D "Go to definition")
           - This field is used to store the name of the subject. It is defined as a 10-character alphanumeric field.
         - **WS-MEDIA**: [`PIC 9(02)V99`](command:_github.copilot.openSymbolFromReferences?%5B%22PIC%209(02)V99%22%2C%5B%7B%22uri%22%3A%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2Fworkspaces%2Fcobol-apps%2FTraining%20Apps%2FPG08MEDIA.cbl%22%2C%22external%22%3A%22file%3A%2F%2F%2Fworkspaces%2Fcobol-apps%2FTraining%2520Apps%2FPG08MEDIA.cbl%22%2C%22path%22%3A%22%2Fworkspaces%2Fcobol-apps%2FTraining%20Apps%2FPG08MEDIA.cbl%22%2C%22scheme%22%3A%22file%22%7D%2C%22pos%22%3A%7B%22line%22%3A12%2C%22character%22%3A34%7D%7D%5D%5D "Go to definition")
           - This field is used to store the average grade for the subject. It is defined as a numeric field with 2 digits before the decimal point and 2 digits after the decimal point.

#### Pseudo Logic

1. **Initialize Program**
   - The program starts execution.

2. **Input Student's Name**
   - The program prompts the user to input the student's name and stores it in [`WS-NOME-ALUNO`](command:_github.copilot.openSymbolFromReferences?%5B%22WS-NOME-ALUNO%22%2C%5B%7B%22uri%22%3A%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2Fworkspaces%2Fcobol-apps%2FTraining%20Apps%2FPG08MEDIA.cbl%22%2C%22external%22%3A%22file%3A%2F%2F%2Fworkspaces%2Fcobol-apps%2FTraining%2520Apps%2FPG08MEDIA.cbl%22%2C%22path%22%3A%22%2Fworkspaces%2Fcobol-apps%2FTraining%20Apps%2FPG08MEDIA.cbl%22%2C%22scheme%22%3A%22file%22%7D%2C%22pos%22%3A%7B%22line%22%3A11%2C%22character%22%3A10%7D%7D%5D%5D "Go to definition").

3. **Iterate Through Subjects**
   - The program uses [`WS-I`](command:_github.copilot.openSymbolFromReferences?%5B%22WS-I%22%2C%5B%7B%22uri%22%3A%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2Fworkspaces%2Fcobol-apps%2FTraining%20Apps%2FPG08MEDIA.cbl%22%2C%22external%22%3A%22file%3A%2F%2F%2Fworkspaces%2Fcobol-apps%2FTraining%2520Apps%2FPG08MEDIA.cbl%22%2C%22path%22%3A%22%2Fworkspaces%2Fcobol-apps%2FTraining%20Apps%2FPG08MEDIA.cbl%22%2C%22scheme%22%3A%22file%22%7D%2C%22pos%22%3A%7B%22line%22%3A11%2C%22character%22%3A10%7D%7D%5D%5D "Go to definition") as a counter to iterate through multiple subjects.

4. **Input Subject Name and Grades**
   - For each subject, the program prompts the user to input the subject name and grades.

5. **Calculate Average Grade**
   - The program calculates the average grade for each subject and stores it in [`WS-MEDIA`](command:_github.copilot.openSymbolFromReferences?%5B%22WS-MEDIA%22%2C%5B%7B%22uri%22%3A%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2Fworkspaces%2Fcobol-apps%2FTraining%20Apps%2FPG08MEDIA.cbl%22%2C%22external%22%3A%22file%3A%2F%2F%2Fworkspaces%2Fcobol-apps%2FTraining%2520Apps%2FPG08MEDIA.cbl%22%2C%22path%22%3A%22%2Fworkspaces%2Fcobol-apps%2FTraining%20Apps%2FPG08MEDIA.cbl%22%2C%22scheme%22%3A%22file%22%7D%2C%22pos%22%3A%7B%22line%22%3A11%2C%22character%22%3A10%7D%7D%5D%5D "Go to definition").

6. **Display Results**
   - The program displays the student's name, subject name, and average grade.

7. **Repeat or Terminate Program**
   - The program checks [`WS-RPT`](command:_github.copilot.openSymbolFromReferences?%5B%22WS-RPT%22%2C%5B%7B%22uri%22%3A%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2Fworkspaces%2Fcobol-apps%2FTraining%20Apps%2FPG08MEDIA.cbl%22%2C%22external%22%3A%22file%3A%2F%2F%2Fworkspaces%2Fcobol-apps%2FTraining%2520Apps%2FPG08MEDIA.cbl%22%2C%22path%22%3A%22%2Fworkspaces%2Fcobol-apps%2FTraining%20Apps%2FPG08MEDIA.cbl%22%2C%22scheme%22%3A%22file%22%7D%2C%22pos%22%3A%7B%22line%22%3A11%2C%22character%22%3A10%7D%7D%5D%5D "Go to definition") to determine whether to repeat the process for another student or terminate the program.

#### Example Implementation (Pseudo Code)

```plaintext
START PROGRAM
  DISPLAY "INSIRA O NOME DO ALUNO: "
  ACCEPT WS-NOME-ALUNO

  PERFORM UNTIL WS-I > NUMBER-OF-SUBJECTS
    DISPLAY "INSIRA O NOME DA MATERIA: "
    ACCEPT WS-NOME-MATERIA
    DISPLAY "INSIRA AS NOTAS: "
    ACCEPT GRADES
    COMPUTE WS-MEDIA = AVERAGE(GRADES)
    DISPLAY "MEDIA DA MATERIA: " WS-MEDIA
    ADD 1 TO WS-I
  END-PERFORM

  DISPLAY "DESEJA REPETIR? (S/N): "
  ACCEPT WS-RPT
  IF WS-RPT = 'S'
    PERFORM INITIALIZE PROGRAM
  ELSE
    STOP RUN
END PROGRAM
```

#### Fields and Variables

- **WS-NOME-ALUNO**: [`PIC X(15)`](command:_github.copilot.openSymbolFromReferences?%5B%22PIC%20X(15)%22%2C%5B%7B%22uri%22%3A%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2Fworkspaces%2Fcobol-apps%2FTraining%20Apps%2FPG08MEDIA.cbl%22%2C%22external%22%3A%22file%3A%2F%2F%2Fworkspaces%2Fcobol-apps%2FTraining%2520Apps%2FPG08MEDIA.cbl%22%2C%22path%22%3A%22%2Fworkspaces%2Fcobol-apps%2FTraining%20Apps%2FPG08MEDIA.cbl%22%2C%22scheme%22%3A%22file%22%7D%2C%22pos%22%3A%7B%22line%22%3A11%2C%22character%22%3A34%7D%7D%5D%5D "Go to definition")
  - This field is used to store the student's name. It is defined as a 15-character alphanumeric field.
- **WS-I**: [`PIC 9(01)`](command:_github.copilot.openSymbolFromReferences?%5B%22PIC%209(01)%22%2C%5B%7B%22uri%22%3A%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2Fworkspaces%2Fcobol-apps%2FTraining%20Apps%2FPG08MEDIA.cbl%22%2C%22external%22%3A%22file%3A%2F%2F%2Fworkspaces%2Fcobol-apps%2FTraining%2520Apps%2FPG08MEDIA.cbl%22%2C%22path%22%3A%22%2Fworkspaces%2Fcobol-apps%2FTraining%20Apps%2FPG08MEDIA.cbl%22%2C%22scheme%22%3A%22file%22%7D%2C%22pos%22%3A%7B%22line%22%3A12%2C%22character%22%3A34%7D%7D%5D%5D "Go to definition")
  - This field is used as a counter for iteration. It is defined as a 1-digit numeric field.
- **WS-RPT**: [`PIC X(01)`](command:_github.copilot.openSymbolFromReferences?%5B%22PIC%20X(01)%22%2C%5B%7B%22uri%22%3A%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2Fworkspaces%2Fcobol-apps%2FTraining%20Apps%2FPG08MEDIA.cbl%22%2C%22external%22%3A%22file%3A%2F%2F%2Fworkspaces%2Fcobol-apps%2FTraining%2520Apps%2FPG08MEDIA.cbl%22%2C%22path%22%3A%22%2Fworkspaces%2Fcobol-apps%2FTraining%20Apps%2FPG08MEDIA.cbl%22%2C%22scheme%22%3A%22file%22%7D%2C%22pos%22%3A%7B%22line%22%3A11%2C%22character%22%3A34%7D%7D%5D%5D "Go to definition")
  - This field is used to store a single character, possibly for a repeat prompt. It is defined as a 1-character alphanumeric field.
- **WS-MATERIA**
  - **WS-NOME-MATERIA**: [`PIC X(10)`](command:_github.copilot.openSymbolFromReferences?%5B%22PIC%20X(10)%22%2C%5B%7B%22uri%22%3A%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2Fworkspaces%2Fcobol-apps%2FTraining%20Apps%2FPG08MEDIA.cbl%22%2C%22external%22%3A%22file%3A%2F%2F%2Fworkspaces%2Fcobol-apps%2FTraining%2520Apps%2FPG08MEDIA.cbl%22%2C%22path%22%3A%22%2Fworkspaces%2Fcobol-apps%2FTraining%20Apps%2FPG08MEDIA.cbl%22%2C%22scheme%22%3A%22file%22%7D%2C%22pos%22%3A%7B%22line%22%3A11%2C%22character%22%3A34%7D%7D%5D%5D "Go to definition")
    - This field is used to store the name of the subject. It is defined as a 10-character alphanumeric field.
  - **WS-MEDIA**: [`PIC 9(02)V99`](command:_github.copilot.openSymbolFromReferences?%5B%22PIC%209(02)V99%22%2C%5B%7B%22uri%22%3A%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2Fworkspaces%2Fcobol-apps%2FTraining%20Apps%2FPG08MEDIA.cbl%22%2C%22external%22%3A%22file%3A%2F%2F%2Fworkspaces%2Fcobol-apps%2FTraining%2520Apps%2FPG08MEDIA.cbl%22%2C%22path%22%3A%22%2Fworkspaces%2Fcobol-apps%2FTraining%20Apps%2FPG08MEDIA.cbl%22%2C%22scheme%22%3A%22file%22%7D%2C%22pos%22%3A%7B%22line%22%3A12%2C%22character%22%3A34%7D%7D%5D%5D "Go to definition")
    - This field is used to store the average grade for the subject. It is defined as a numeric field with 2 digits before the decimal point and 2 digits after the decimal point.

This documentation provides an overview of the program structure, the purpose of each section, and a high-level pseudo logic of how the average grade calculation is performed and displayed. The program is straightforward and serves as a basic example of data handling, iteration, and arithmetic operations in COBOL.

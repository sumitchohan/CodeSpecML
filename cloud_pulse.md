sumitchohan: @workspace  Create a directory structure of all code files in the current codebase and include a brief description of each code file


Workspace: Here is a directory structure of all code files in the current codebase along with a brief description of each code file:

```
src/
├── apis/
│   ├── accountAPI.tsx                # API interactions related to account management
│   ├── AddRegionAPI.tsx              # API interactions for adding regions
│   ├── azureAccountCreation.tsx      # API interactions for Azure account creation
│   ├── clientAPI.tsx                 # API interactions for client-related operations
│   ├── createAccount.tsx             # API interactions for creating accounts
│   ├── dasboardAPI.tsx               # API interactions for dashboard data
│   ├── deleteAccountAPI.tsx          # API interactions for deleting accounts
│   ├── getTenentAPI.tsx              # API interactions for retrieving tenant information
│   ├── governanceAPI.tsx             # API interactions for governance-related data
│   ├── jiraTicketAPI.tsx             # API interactions for Jira ticket management
│   ├── MarkIgnoreAPI.tsx             # API interactions for marking items to ignore
│   ├── policyDeployAPI.tsx           # API interactions for policy deployment
│   ├── removeExceptionListAPI.tsx    # API interactions for removing items from exception list
│   ├── removeIgnoreListAPI.tsx       # API interactions for removing items from ignore list
├── App.css                           # Main CSS file for the application
├── App.tsx                           # Main application component
├── assets/
│   ├── fonts/                        # Directory containing font files
│   ├── icons/                        # Directory containing icon files
├── components/
│   ├── addRegion.tsx                 # Component for adding regions
│   ├── awsAccountSetup.tsx           # Component for AWS account setup
│   ├── configuration.tsx             # Component for configuration settings
│   ├── createTicketOptimiser.tsx     # Component for creating tickets with optimiser
│   ├── customSelect.tsx              # Custom select component
│   ├── governance.tsx                # Component for governance-related UI
│   ├── recommendation.tsx            # Component for displaying recommendations
│   ├── resourceList.tsx              # Component for displaying a list of resources
│   ├── resourceNotificationErrorPop.tsx # Component for resource notification error pop-up
│   ├── resourceNotificationPop.tsx   # Component for resource notification pop-up
│   ├── selectCloud.tsx               # Component for selecting cloud provider
│   ├── azureAccountSetup.tsx         # Component for Azure account setup
├── components-css/
│   ├── resourceList.css              # CSS for resource list component
├── index.css                         # Global CSS file
├── main.tsx                          # Main entry point for the application
├── pages/
│   ├── createAccountPage.tsx         # Page component for creating accounts
├── services/                         # Directory for service-related files
├── utils/                            # Directory for utility functions and interfaces
│   ├── constants.ts                  # File containing constant values
│   ├── dateUtils.ts                  # Utility functions for date manipulation
│   ├── interfaces.ts                 # TypeScript interfaces used across the application
├── vite-env.d.ts                     # TypeScript environment declarations for Vite
```

This structure provides an overview of the code files and their purposes within the codebase.

sumitchohan: @workspace create a plant uml diagram showing solution structure

Workspace: Here is a PlantUML diagram representing the structure of your solution:

```plantuml
@startuml

package "Root" {
    .eslintrc.cjs
    .gitignore
    index.html
    package.json
    postcss.config.js
    tailwind.config.js
    tsconfig.json
    tsconfig.node.json
    vite.config.ts

    package "public" {
        configuration.json
        nagarro-cloud-pulse-multiple-account-template-dev.json
        temp-account-data.json
    }

    package "src" {
        App.css
        App.tsx
        index.css
        main.tsx
        vite-env.d.ts

        package "apis" {
            accountAPI.tsx
            AddRegionAPI.tsx
            azureAccountCreation.tsx
            clientAPI.tsx
            createAccount.tsx
            // other API files...
        }

        package "assets" {
            package "fonts" {
                // font files...
            }
            package "icons" {
                // icon files...
            }
        }

        package "components" {
            createTicketOptimiser.tsx
            AwsFormComponents/enviromentAlias.tsx
            pillarCard.tsx
            createTicket.tsx
            // other component files...
        }

        package "components-css" {
            PolicyTray.css
            // other CSS files...
        }

        package "pages" {
            // page files...
        }

        package "services" {
            // service files...
        }

        package "utils" {
            constants.ts
            interfaces.ts
            // other utility files...
        }
    }
}

@enduml
```

This diagram provides a high-level overview of the directory structure and the files within each directory. You can visualize this diagram using any PlantUML tool or plugin.

sumitchohan: @workspace Provide explanation of above diagram

Workspace: The provided diagram represents the structure of your project workspace. Here's a detailed explanation of each part:

### Root Directory
- **Configuration and Metadata Files**:
  - [`.eslintrc.cjs`](command:_github.copilot.openRelativePath?%5B%7B%22scheme%22%3A%22file%22%2C%22authority%22%3A%22%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2F.eslintrc.cjs%22%2C%22query%22%3A%22%22%2C%22fragment%22%3A%22%22%7D%5D "/Users/sumit/Downloads/Frontend/NAGISVPR/.eslintrc.cjs"): ESLint configuration file.
  - [`.gitignore`](command:_github.copilot.openRelativePath?%5B%7B%22scheme%22%3A%22file%22%2C%22authority%22%3A%22%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2F.gitignore%22%2C%22query%22%3A%22%22%2C%22fragment%22%3A%22%22%7D%5D "/Users/sumit/Downloads/Frontend/NAGISVPR/.gitignore"): Specifies files and directories to be ignored by Git.
  - [`index.html`](command:_github.copilot.openRelativePath?%5B%7B%22scheme%22%3A%22file%22%2C%22authority%22%3A%22%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Findex.html%22%2C%22query%22%3A%22%22%2C%22fragment%22%3A%22%22%7D%5D "/Users/sumit/Downloads/Frontend/NAGISVPR/index.html"): The main HTML file for the application.
  - [`package.json`](command:_github.copilot.openRelativePath?%5B%7B%22scheme%22%3A%22file%22%2C%22authority%22%3A%22%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fpackage.json%22%2C%22query%22%3A%22%22%2C%22fragment%22%3A%22%22%7D%5D "/Users/sumit/Downloads/Frontend/NAGISVPR/package.json"): Contains metadata about the project and its dependencies.
  - [`postcss.config.js`](command:_github.copilot.openRelativePath?%5B%7B%22scheme%22%3A%22file%22%2C%22authority%22%3A%22%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fpostcss.config.js%22%2C%22query%22%3A%22%22%2C%22fragment%22%3A%22%22%7D%5D "/Users/sumit/Downloads/Frontend/NAGISVPR/postcss.config.js"): Configuration for PostCSS.
  - [`tailwind.config.js`](command:_github.copilot.openRelativePath?%5B%7B%22scheme%22%3A%22file%22%2C%22authority%22%3A%22%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Ftailwind.config.js%22%2C%22query%22%3A%22%22%2C%22fragment%22%3A%22%22%7D%5D "/Users/sumit/Downloads/Frontend/NAGISVPR/tailwind.config.js"): Configuration for Tailwind CSS.
  - [`tsconfig.json`](command:_github.copilot.openRelativePath?%5B%7B%22scheme%22%3A%22file%22%2C%22authority%22%3A%22%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Ftsconfig.json%22%2C%22query%22%3A%22%22%2C%22fragment%22%3A%22%22%7D%5D "/Users/sumit/Downloads/Frontend/NAGISVPR/tsconfig.json"): TypeScript configuration file.
  - [`tsconfig.node.json`](command:_github.copilot.openRelativePath?%5B%7B%22scheme%22%3A%22file%22%2C%22authority%22%3A%22%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Ftsconfig.node.json%22%2C%22query%22%3A%22%22%2C%22fragment%22%3A%22%22%7D%5D "/Users/sumit/Downloads/Frontend/NAGISVPR/tsconfig.node.json"): TypeScript configuration for Node.js.
  - [`vite.config.ts`](command:_github.copilot.openRelativePath?%5B%7B%22scheme%22%3A%22file%22%2C%22authority%22%3A%22%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fvite.config.ts%22%2C%22query%22%3A%22%22%2C%22fragment%22%3A%22%22%7D%5D "/Users/sumit/Downloads/Frontend/NAGISVPR/vite.config.ts"): Configuration for Vite, a build tool.

### [`public`](command:_github.copilot.openRelativePath?%5B%7B%22scheme%22%3A%22file%22%2C%22authority%22%3A%22%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fpublic%22%2C%22query%22%3A%22%22%2C%22fragment%22%3A%22%22%7D%5D "/Users/sumit/Downloads/Frontend/NAGISVPR/public") Directory
- Contains static files that are served directly:
  - `configuration.json`
  - `nagarro-cloud-pulse-multiple-account-template-dev.json`
  - [`temp-account-data.json`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2Fgovernance.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2Fgovernance.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2Fgovernance.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A75%2C%22character%22%3A4%7D%5D "src/components/governance.tsx")

### [`src`](command:_github.copilot.openRelativePath?%5B%7B%22scheme%22%3A%22file%22%2C%22authority%22%3A%22%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%22%2C%22query%22%3A%22%22%2C%22fragment%22%3A%22%22%7D%5D "/Users/sumit/Downloads/Frontend/NAGISVPR/src") Directory
- **Source Code**:
  - `App.css`: CSS for the main App component.
  - `App.tsx`: Main App component.
  - [`index.css`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FDashboardChartComponent%2FpolicyComplianceChart.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FDashboardChartComponent%2FpolicyComplianceChart.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FDashboardChartComponent%2FpolicyComplianceChart.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A30%2C%22character%22%3A47%7D%5D "src/components/DashboardChartComponent/policyComplianceChart.tsx"): Global CSS file.
  - `main.tsx`: Entry point for the React application.
  - `vite-env.d.ts`: TypeScript declaration file for Vite.

- **Subdirectories**:
  - [`apis/`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FclientAPI.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A0%2C%22character%22%3A0%7D%5D "src/apis/clientAPI.tsx"): Contains API-related files.
    - [`accountAPI.tsx`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FaccountAPI.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A0%2C%22character%22%3A0%7D%5D "src/apis/accountAPI.tsx")
    - `AddRegionAPI.tsx`
    - `azureAccountCreation.tsx`
    - [`clientAPI.tsx`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FclientAPI.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A0%2C%22character%22%3A0%7D%5D "src/apis/clientAPI.tsx")
    - `createAccount.tsx`
    - Other API files...
  - [`assets/`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fassets%2Ficons%2Ficon-tool-tip.svg%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A0%2C%22character%22%3A0%7D%5D "src/assets/icons/icon-tool-tip.svg"): Contains static assets like fonts and icons.
    - `fonts/`
    - [`icons/`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fassets%2Ficons%2Ficon-tool-tip.svg%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A0%2C%22character%22%3A0%7D%5D "src/assets/icons/icon-tool-tip.svg")
  - `components/`: Contains React components.
    - [`complianceStatusCard.tsx`](command:_github.copilot.openRelativePath?%5B%7B%22scheme%22%3A%22file%22%2C%22authority%22%3A%22%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FcomplianceStatusCard.tsx%22%2C%22query%22%3A%22%22%2C%22fragment%22%3A%22%22%7D%5D "/Users/sumit/Downloads/Frontend/NAGISVPR/src/components/complianceStatusCard.tsx"): Component for displaying compliance status.
    - [`governance.tsx`](command:_github.copilot.openRelativePath?%5B%7B%22scheme%22%3A%22file%22%2C%22authority%22%3A%22%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2Fgovernance.tsx%22%2C%22query%22%3A%22%22%2C%22fragment%22%3A%22%22%7D%5D "/Users/sumit/Downloads/Frontend/NAGISVPR/src/components/governance.tsx"): Component for governance-related UI.
    - [`DashboardChartComponent/policyComplianceChart.tsx`](command:_github.copilot.openRelativePath?%5B%7B%22scheme%22%3A%22file%22%2C%22authority%22%3A%22%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FDashboardChartComponent%2FpolicyComplianceChart.tsx%22%2C%22query%22%3A%22%22%2C%22fragment%22%3A%22%22%7D%5D "/Users/sumit/Downloads/Frontend/NAGISVPR/src/components/DashboardChartComponent/policyComplianceChart.tsx"): Component for policy compliance chart.
    - [`pillarCard.tsx`](command:_github.copilot.openRelativePath?%5B%7B%22scheme%22%3A%22file%22%2C%22authority%22%3A%22%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FpillarCard.tsx%22%2C%22query%22%3A%22%22%2C%22fragment%22%3A%22%22%7D%5D "/Users/sumit/Downloads/Frontend/NAGISVPR/src/components/pillarCard.tsx"): Component for displaying pillar information.
    - Other component files...
  - `components-css/`: Contains CSS files for components.
    - `PolicyTray.css`
    - Other CSS files...
  - `pages/`: Contains page components.
  - [`services/`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FDashboardChartComponent%2FpolicyComplianceChart.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FDashboardChartComponent%2FpolicyComplianceChart.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FDashboardChartComponent%2FpolicyComplianceChart.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A24%2C%22character%22%3A2%7D%5D "src/components/DashboardChartComponent/policyComplianceChart.tsx"): Contains service files.
  - [`utils/`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Futils%2Fconstants.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A0%2C%22character%22%3A0%7D%5D "src/utils/constants.tsx"): Contains utility files.
    - [`constants.ts`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Futils%2Fconstants.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A0%2C%22character%22%3A0%7D%5D "src/utils/constants.tsx"): Contains constants used across the application.
    - [`interfaces.ts`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Futils%2Finterfaces.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A0%2C%22character%22%3A0%7D%5D "src/utils/interfaces.tsx"): Contains TypeScript interfaces.
    - Other utility files...

### Explanation of Code Excerpts
- **[`complianceStatusCard.tsx`](command:_github.copilot.openRelativePath?%5B%7B%22scheme%22%3A%22file%22%2C%22authority%22%3A%22%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FcomplianceStatusCard.tsx%22%2C%22query%22%3A%22%22%2C%22fragment%22%3A%22%22%7D%5D "/Users/sumit/Downloads/Frontend/NAGISVPR/src/components/complianceStatusCard.tsx")**:
  - Uses [`Tippy`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FcomplianceStatusCard.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FcomplianceStatusCard.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FcomplianceStatusCard.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A3%2C%22character%22%3A0%7D%5D "src/components/complianceStatusCard.tsx") for tooltips.
  - Displays an icon with a tooltip and two buttons for "Deployed" and "Total" states.

- **[`governance.tsx`](command:_github.copilot.openRelativePath?%5B%7B%22scheme%22%3A%22file%22%2C%22authority%22%3A%22%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2Fgovernance.tsx%22%2C%22query%22%3A%22%22%2C%22fragment%22%3A%22%22%7D%5D "/Users/sumit/Downloads/Frontend/NAGISVPR/src/components/governance.tsx")**:
  - Uses [`PillarCard`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FpillarCard.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FpillarCard.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FpillarCard.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A22%2C%22character%22%3A0%7D%5D "src/components/pillarCard.tsx") components to display information about different pillars.
  - Includes a [`Loader`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2Floader.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A7%2C%22character%22%3A0%7D%5D "src/components/loader.tsx") component for loading states.
  - Uses [`CustomSearchSelect`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FcustomSearchSelect.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A13%2C%22character%22%3A0%7D%5D "src/components/customSearchSelect.tsx") for selecting account aliases.

- **[`policyComplianceChart.tsx`](command:_github.copilot.openRelativePath?%5B%7B%22scheme%22%3A%22file%22%2C%22authority%22%3A%22%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FDashboardChartComponent%2FpolicyComplianceChart.tsx%22%2C%22query%22%3A%22%22%2C%22fragment%22%3A%22%22%7D%5D "/Users/sumit/Downloads/Frontend/NAGISVPR/src/components/DashboardChartComponent/policyComplianceChart.tsx")**:
  - Imports various chart components from `recharts`.
  - Uses [`Tippy`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FcomplianceStatusCard.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FcomplianceStatusCard.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FcomplianceStatusCard.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A3%2C%22character%22%3A0%7D%5D "src/components/complianceStatusCard.tsx") for tooltips and displays an icon.

- **[`pillarCard.tsx`](command:_github.copilot.openRelativePath?%5B%7B%22scheme%22%3A%22file%22%2C%22authority%22%3A%22%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FpillarCard.tsx%22%2C%22query%22%3A%22%22%2C%22fragment%22%3A%22%22%7D%5D "/Users/sumit/Downloads/Frontend/NAGISVPR/src/components/pillarCard.tsx")**:
  - Uses [`PieChart`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FpillarCard.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FpillarCard.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FpillarCard.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A2%2C%22character%22%3A0%7D%5D "src/components/pillarCard.tsx") from `recharts` to display data.
  - Defines colors for different states.

This structure helps in organizing the project efficiently, making it easier to maintain and scale.

sumitchohan: @workspace generate plant uml component diagrams for all components in current code base

Workspace: I'm sorry, I can't answer that question with what I currently know about your workspace.

sumitchohan: @workspace generate plant uml component diagrams for all components in current code base

Workspace: To generate PlantUML component diagrams for all components in your codebase, you can follow these steps:

1. **Install PlantUML Extension**: Ensure you have the PlantUML extension installed in Visual Studio Code.
2. **Create a PlantUML File**: Create a new file, e.g., [`components.puml`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents-css%2FPolicyTray.css%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A0%2C%22character%22%3A0%7D%5D "src/components-css/PolicyTray.css"), in your workspace.
3. **Define Components**: Add the components based on your workspace structure.

Here is an example of how you can define the components in your [`components.puml`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents-css%2FPolicyTray.css%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A0%2C%22character%22%3A0%7D%5D "src/components-css/PolicyTray.css") file:

```plantuml
@startuml

package "src/apis" {
    [accountAPI]
    [AddRegionAPI]
    [azureAccountCreation]
    [clientAPI]
    [createAccount]
}

package "src/components" {
    [awsAccountSetup]
    [azureAccountSetup]
    [policyList]
    [policyDetails]
    [AwsFormComponents/enviromentAlias]
    [recommendation]
    [createTicket]
    [awsCLI]
    [DashboardChartComponent/policyComplianceChart]
    [DashboardChartComponent/allCostSavingTable]
}

package "src/pages" {
    [pagination]
}

package "src/services" {
    [authService]
}

package "src/utils" {
    [interfaces]
    [constants]
}

@enduml
```

4. **Render the Diagram**: Open the `components.puml` file in Visual Studio Code and use the PlantUML extension to render the diagram.

This will give you a basic component diagram. You can further enhance it by adding relationships and dependencies between the components if needed.

sumitchohan: @workspace create another plant uml diagram showing relationships among all business entities and data models

Workspace: To create a PlantUML diagram showing relationships among all business entities and data models in your codebase, you can follow these steps:

1. **Identify Entities and Data Models**: Based on the provided code excerpts, identify the key entities and data models.
2. **Create a PlantUML File**: Create a new file, e.g., [`entities.puml`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents-css%2FPolicyTray.css%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A0%2C%22character%22%3A0%7D%5D "src/components-css/PolicyTray.css")) in your workspace.
3. **Define Entities and Relationships**: Add the entities and their relationships based on your workspace structure.

Here is an example of how you can define the entities and their relationships in your [`entities.puml`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents-css%2FPolicyTray.css%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A0%2C%22character%22%3A0%7D%5D "src/components-css/PolicyTray.css")) file:

```plantuml
@startuml

package "src/utils/interfaces" {
    class Account {
        +id: string
        +name: string
        +type: string
    }

    class Policy {
        +id: string
        +name: string
        +description: string
    }

    class PolicySummaryGovernance {
        +policyId: string
        +summary: string
    }
}

package "src/apis" {
    class accountAPI {
        +getAllAccounts(): Account[]
    }

    class clientAPI {
        +getAccountList(): Account[]
        +getAllPolicy(): Policy[]
    }

    class governanceAPI {
        +getGovernance(): PolicySummaryGovernance[]
    }

    class policyPageEmail {
        +getPolicyEmail(): string
        +updatePolicyEmail(email: string): void
    }

    class policyDeployAPI {
        +policyDeployFunction(policy: Policy): void
    }

    class jiraTicketAPI {
        +creatJiraTicket(policy: Policy): void
    }
}

package "src/components" {
    class governance {
        +props: GovernanceProps
    }

    class createTicketOptimiser {
        +props: TicketRunProps
    }

    class pillarCard {
        +props: PillarCardProps
    }

    class allCostSavingTable {
        +props: CostSavingsCardProps
    }

    class nonCompliantPoliciesChart {
        +data: DataItem[]
    }

    class policyDetails {
        +props: PolicyRunProps
    }

    class createTicket {
        +props: TicketRunProps
    }
}

Account --> PolicySummaryGovernance : contains
Policy --> PolicySummaryGovernance : summarized by
accountAPI --> Account : returns
clientAPI --> Account : returns
clientAPI --> Policy : returns
governanceAPI --> PolicySummaryGovernance : returns
policyPageEmail --> Policy : updates
policyDeployAPI --> Policy : deploys
jiraTicketAPI --> Policy : creates ticket for

@enduml
```

4. **Render the Diagram**: Open the `entities.puml` file in Visual Studio Code and use the PlantUML extension to render the diagram.

This will give you a basic entity-relationship diagram. You can further enhance it by adding more details and relationships as needed.

sumitchohan: @workspace Create a plantuml diagram for all classes in current codebase


Workspace: I'm sorry, I can't answer that question with what I currently know about your workspace.

sumitchohan: @workspace Create a plantuml diagram for all classes in current codebase

Workspace: To create a PlantUML diagram for all classes in your codebase, you can follow these steps:

1. **Install PlantUML**: Ensure you have PlantUML installed. You can install it as a Visual Studio Code extension or use it as a standalone tool.

2. **Generate UML Diagram**: Use a script to parse your TypeScript files and generate the PlantUML diagram. Below is an example script to help you get started. This script will scan your [`src`](command:_github.copilot.openRelativePath?%5B%7B%22scheme%22%3A%22file%22%2C%22authority%22%3A%22%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%22%2C%22query%22%3A%22%22%2C%22fragment%22%3A%22%22%7D%5D "/Users/sumit/Downloads/Frontend/NAGISVPR/src") directory for TypeScript files and generate a basic PlantUML diagram.

```bash
#!/bin/bash

# Create a PlantUML file
output="diagram.puml"
echo "@startuml" > $output

# Function to extract class names and their relationships
extract_classes() {
  grep -E "class |interface " $1 | while read -r line ; do
    class_name=$(echo $line | awk '{print $2}')
    echo "class $class_name" >> $output
  done
}

# Recursively find all TypeScript files and extract classes
find src -name "*.ts" -o -name "*.tsx" | while read -r file ; do
  extract_classes $file
done

echo "@enduml" >> $output
```

3. **Run the Script**: Save the script as `generate_uml.sh` and run it in your terminal:

```sh
chmod +x generate_uml.sh
./generate_uml.sh
```

4. **Render the Diagram**: Open the generated `diagram.puml` file in Visual Studio Code with the PlantUML extension, or use the PlantUML tool to render the diagram.

This will give you a basic UML diagram of the classes in your codebase. You can further enhance the script to include relationships, methods, and properties as needed.

sumitchohan: @workspace Create a plantuml diagram for all classes in current codebase


Workspace: To create a PlantUML diagram for all classes in your codebase, you can use the following script. This script will scan your [`src`](command:_github.copilot.openRelativePath?%5B%7B%22scheme%22%3A%22file%22%2C%22authority%22%3A%22%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%22%2C%22query%22%3A%22%22%2C%22fragment%22%3A%22%22%7D%5D "/Users/sumit/Downloads/Frontend/NAGISVPR/src") directory for TypeScript files and generate a basic PlantUML diagram.

1. **Create the Script**: Save the following script as `generate_uml.sh` in the root of your workspace.

```bash
#!/bin/bash

# Create a PlantUML file
output="diagram.puml"
echo "@startuml" > $output

# Function to extract class names and their relationships
extract_classes() {
  grep -E "class |interface " $1 | while read -r line ; do
    class_name=$(echo $line | awk '{print $2}')
    echo "class $class_name" >> $output
  done
}

# Recursively find all TypeScript files and extract classes
find src -name "*.ts" -o -name "*.tsx" | while read -r file ; do
  extract_classes $file
done

echo "@enduml" >> $output
```

2. **Run the Script**: Make the script executable and run it in your terminal:

```sh
chmod +x generate_uml.sh
./generate_uml.sh
```

3. **Render the Diagram**: Open the generated `diagram.puml` file in Visual Studio Code with the PlantUML extension, or use the PlantUML tool to render the diagram.

This will give you a basic UML diagram of the classes in your codebase. You can further enhance the script to include relationships, methods, and properties as needed.

sumitchohan: @workspace generate plant uml diagram showing all use cases in current codebase 


Workspace: To generate a PlantUML diagram showing all use cases in your current codebase, you can follow these steps:

1. **Create a PlantUML File**: Save the following content as `use_cases.puml` in the root of your workspace.

```plantuml
@startuml

!define RECTANGLE class

RECTANGLE "User" as User

RECTANGLE "AccountAPI" as AccountAPI
RECTANGLE "AddRegionAPI" as AddRegionAPI
RECTANGLE "AzureAccountCreation" as AzureAccountCreation
RECTANGLE "ClientAPI" as ClientAPI
RECTANGLE "CreateAccount" as CreateAccount
RECTANGLE "CreateAccountCheckStack" as CreateAccountCheckStack
RECTANGLE "GetCommentRI" as GetCommentRI
RECTANGLE "GetTenentAPI" as GetTenentAPI
RECTANGLE "GovernanceAPI" as GovernanceAPI
RECTANGLE "HighestCostSavings" as HighestCostSavings
RECTANGLE "JiraTicketAPI" as JiraTicketAPI
RECTANGLE "PolicyGetEmail" as PolicyGetEmail
RECTANGLE "PolicyPageAPIs" as PolicyPageAPIs
RECTANGLE "PostCommentRI" as PostCommentRI
RECTANGLE "RemoveIgnoreListAPI" as RemoveIgnoreListAPI

User --> Account

API

 : "Uses"
User --> AddRegionAPI : "Uses"
User --> AzureAccountCreation : "Uses"
User --> ClientAPI : "Uses"
User --> CreateAccount : "Uses"
User --> CreateAccountCheckStack : "Uses"
User --> GetCommentRI : "Uses"
User --> GetTenentAPI : "Uses"
User --> GovernanceAPI : "Uses"
User --> HighestCostSavings : "Uses"
User --> JiraTicketAPI : "Uses"
User --> PolicyGetEmail : "Uses"
User --> PolicyPageAPIs : "Uses"
User --> PostCommentRI : "Uses"
User --> RemoveIgnoreListAPI : "Uses"

@enduml
```

2. **Render the Diagram**: Open the `use_cases.puml` file in Visual Studio Code with the PlantUML extension, or use the PlantUML tool to render the diagram.

This will give you a basic UML diagram of the use cases in your codebase. You can further enhance the diagram by adding more details about the interactions and relationships between the components.

sumitchohan: generate detailed documentation for current code file including all methods, fields, properties and summary of their implementation including pseudo logic

GitHub Copilot: ### Detailed Documentation for [`getFilterDataApi.tsx`](command:_github.copilot.openRelativePath?%5B%7B%22scheme%22%3A%22file%22%2C%22authority%22%3A%22%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FrispApis%2FgetFilterDataApi.tsx%22%2C%22query%22%3A%22%22%2C%22fragment%22%3A%22%22%7D%5D "/Users/sumit/Downloads/Frontend/NAGISVPR/src/apis/rispApis/getFilterDataApi.tsx")

#### Summary
This file contains a utility function [`formatDate`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FrispApis%2FgetFilterDataApi.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FrispApis%2FgetFilterDataApi.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FrispApis%2FgetFilterDataApi.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A2%2C%22character%22%3A0%7D%5D "src/apis/rispApis/getFilterDataApi.tsx") and an asynchronous function [`getFilterDataApi`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FrispApis%2FgetFilterDataApi.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FrispApis%2FgetFilterDataApi.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FrispApis%2FgetFilterDataApi.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A14%2C%22character%22%3A0%7D%5D "src/apis/rispApis/getFilterDataApi.tsx") that interacts with an external API to fetch filtered data based on various parameters.

#### Methods

1. **formatDate**
   - **Description**: Formats a [`Date`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22external%22%3A%22file%3A%2F%2F%2FApplications%2FVisual%2520Studio%2520Code.app%2FContents%2FResources%2Fapp%2Fextensions%2Fnode_modules%2Ftypescript%2Flib%2Flib.es2015.symbol.wellknown.d.ts%22%2C%22path%22%3A%22%2FApplications%2FVisual%20Studio%20Code.app%2FContents%2FResources%2Fapp%2Fextensions%2Fnode_modules%2Ftypescript%2Flib%2Flib.es2015.symbol.wellknown.d.ts%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A111%2C%22character%22%3A0%7D%5D "../../../../../Applications/Visual Studio Code.app/Contents/Resources/app/extensions/node_modules/typescript/lib/lib.es2015.symbol.wellknown.d.ts") object into a string in the format `YYYY-MM-DD`.
   - **Parameters**:
     - [`date`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FrispApis%2FgetFilterDataApi.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FrispApis%2FgetFilterDataApi.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FrispApis%2FgetFilterDataApi.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A2%2C%22character%22%3A20%7D%5D "src/apis/rispApis/getFilterDataApi.tsx") (Date): The date to be formatted.
   - **Returns**: A string representing the formatted date.
   - **Pseudo Logic**:
     1. Check if the [`date`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FrispApis%2FgetFilterDataApi.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FrispApis%2FgetFilterDataApi.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FrispApis%2FgetFilterDataApi.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A2%2C%22character%22%3A20%7D%5D "src/apis/rispApis/getFilterDataApi.tsx") is `null`. If so, return an empty string.
     2. Extract the year, month, and day from the [`date`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FrispApis%2FgetFilterDataApi.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FrispApis%2FgetFilterDataApi.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FrispApis%2FgetFilterDataApi.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A2%2C%22character%22%3A20%7D%5D "src/apis/rispApis/getFilterDataApi.tsx").
     3. Format the month and day to ensure they are two digits.
     4. Return the formatted date string in the format `YYYY-MM-DD`.

   ```typescript
   const formatDate = (date: Date): string => {
     if (date == null) {
       return "";
     }

     const year = date.getFullYear();
     const month = String(date.getMonth() + 1).padStart(2, "0"); // Months are zero-based
     const day = String(date.getDate()).padStart(2, "0");

     return `${year}-${month}-${day}`;
   };
   ```

2. **getFilterDataApi**
   - **Description**: Fetches filtered data from an external API based on the provided parameters.
   - **Parameters**:
     - `tenantId` (string): The tenant ID.
     - `usageFrom` (number): The starting usage value.
     - `usageTo` (number): The ending usage value.
     - `location` (string[]): An array of locations.
     - `vmSize` (string[]): An array of VM sizes.
     - `subscription` (string[]): An array of subscriptions.
     - `creationDateFrom` (Date): The starting creation date.
     - `creationDateTo` (Date): The ending creation date.
   - **Returns**: A promise that resolves to the API response.
   - **Pseudo Logic**:
     1. Define the API URL.
     2. Construct the request body with the provided parameters, formatting the dates using `formatDate`.
     3. Make a POST request to the API with the constructed body.
     4. Return the API response.

   ```typescript
   export const getFilterDataApi = async (
     tenantId: string,
     usageFrom: number,
     usageTo: number,
     location: string[],
     vmSize: string[],
     subscription: string[],
     creationDateFrom: Date,
     creationDateTo: Date
   ): Promise<any> => {
     const url =
       "https://aw9cl7kijh.execute-api.us-east-1.amazonaws.com/dev/filter-data";
     try {
       const body = {
         tenantId: tenantId,
         usage: {
           usageFrom: usageFrom,
           usageTo: usageTo,
         },
         location: location,
         vm_size: vmSize,
         subscription: subscription,
         creationDate: {
           creationDateFrom: formatDate(creationDateFrom),
           creationDateTo: formatDate(creationDateTo),
         },
       };

       const response = await axios.post(url, body);
       return response.data;
     } catch (error) {
       console.error("Error fetching filter data:", error);
       throw error;
     }
   };
   ```

#### Fields and Properties
- **API URL**: The URL of the external API endpoint.
  ```typescript
  const url = "https://aw9cl7kijh.execute-api.us-east-1.amazonaws.com/dev/filter-data";
  ```

#### Usage Example
```typescript
import { getFilterDataApi } from './getFilterDataApi';

const fetchData = async () => {
  try {
    const data = await getFilterDataApi(
      'tenant123',
      100,
      200,
      ['us-east-1'],
      ['Standard_B1s'],
      ['sub123'],
      new Date('2023-01-01'),
      new Date('2023-12-31')
    );
    console.log(data);
  } catch (error) {
    console.error('Error:', error);
  }
};

fetchData();
```

This documentation provides a comprehensive overview of the methods, their parameters, return types, and the logic implemented in the [`getFilterDataApi.tsx`](command:_github.copilot.openRelativePath?%5B%7B%22scheme%22%3A%22file%22%2C%22authority%22%3A%22%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FrispApis%2FgetFilterDataApi.tsx%22%2C%22query%22%3A%22%22%2C%22fragment%22%3A%22%22%7D%5D "/Users/sumit/Downloads/Frontend/NAGISVPR/src/apis/rispApis/getFilterDataApi.tsx") file.

sumitchohan: generate detailed documentation for current code file including all methods, fields, properties and summary of their implementation including pseudo logic

GitHub Copilot: ### Documentation for [`setTenureApi.tsx`](command:_github.copilot.openRelativePath?%5B%7B%22scheme%22%3A%22file%22%2C%22authority%22%3A%22%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FrispApis%2FsetTenureApi.tsx%22%2C%22query%22%3A%22%22%2C%22fragment%22%3A%22%22%7D%5D "/Users/sumit/Downloads/Frontend/NAGISVPR/src/apis/rispApis/setTenureApi.tsx")

#### Summary
This file contains a single asynchronous function [`setTenureApi`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FrispApis%2FsetTenureApi.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FrispApis%2FsetTenureApi.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FrispApis%2FsetTenureApi.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A0%2C%22character%22%3A0%7D%5D "src/apis/rispApis/setTenureApi.tsx") that sends a POST request to a specified API endpoint with tenure-related data. The function handles potential errors and returns the response from the API.

#### Function: [`setTenureApi`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FrispApis%2FsetTenureApi.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FrispApis%2FsetTenureApi.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FrispApis%2FsetTenureApi.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A0%2C%22character%22%3A0%7D%5D "src/apis/rispApis/setTenureApi.tsx")

##### Description
The [`setTenureApi`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FrispApis%2FsetTenureApi.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FrispApis%2FsetTenureApi.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FrispApis%2FsetTenureApi.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A0%2C%22character%22%3A0%7D%5D "src/apis/rispApis/setTenureApi.tsx") function is designed to send tenure-related data to a specified API endpoint. It constructs a JSON payload from the provided parameters and sends it via a POST request. The function handles errors by logging them and rethrowing the error.

##### Parameters
- [`vm_id`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FrispApis%2FsetTenureApi.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FrispApis%2FsetTenureApi.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FrispApis%2FsetTenureApi.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A1%2C%22character%22%3A2%7D%5D "src/apis/rispApis/setTenureApi.tsx") (optional, `string`): The ID of the virtual machine.
- [`vm_name`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FrispApis%2FsetTenureApi.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FrispApis%2FsetTenureApi.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FrispApis%2FsetTenureApi.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A2%2C%22character%22%3A2%7D%5D "src/apis/rispApis/setTenureApi.tsx") (optional, `string`): The name of the virtual machine.
- [`subscription_id`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FrispApis%2FsetTenureApi.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FrispApis%2FsetTenureApi.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FrispApis%2FsetTenureApi.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A3%2C%22character%22%3A2%7D%5D "src/apis/rispApis/setTenureApi.tsx") (optional, `string`): The subscription ID associated with the virtual machine.
- [`location`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FrispApis%2FsetTenureApi.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FrispApis%2FsetTenureApi.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FrispApis%2FsetTenureApi.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A4%2C%22character%22%3A2%7D%5D "src/apis/rispApis/setTenureApi.tsx") (optional, `string`): The location of the virtual machine.
- [`tenure`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FrispApis%2FsetTenureApi.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FrispApis%2FsetTenureApi.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FrispApis%2FsetTenureApi.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A5%2C%22character%22%3A2%7D%5D "src/apis/rispApis/setTenureApi.tsx") (optional, `string`): The tenure information to be sent.
- [`selected_by`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FrispApis%2FsetTenureApi.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FrispApis%2FsetTenureApi.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FrispApis%2FsetTenureApi.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A6%2C%22character%22%3A2%7D%5D "src/apis/rispApis/setTenureApi.tsx") (optional, `string`, default: `"User1.5"`): The user who selected the tenure. Defaults to `"User1.5"`.
- [`selected_at`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FrispApis%2FsetTenureApi.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FrispApis%2FsetTenureApi.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FrispApis%2FsetTenureApi.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A7%2C%22character%22%3A2%7D%5D "src/apis/rispApis/setTenureApi.tsx") (optional, `string`, default: [`new Date().toISOString()`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22external%22%3A%22file%3A%2F%2F%2FApplications%2FVisual%2520Studio%2520Code.app%2FContents%2FResources%2Fapp%2Fextensions%2Fnode_modules%2Ftypescript%2Flib%2Flib.es2015.symbol.wellknown.d.ts%22%2C%22path%22%3A%22%2FApplications%2FVisual%20Studio%20Code.app%2FContents%2FResources%2Fapp%2Fextensions%2Fnode_modules%2Ftypescript%2Flib%2Flib.es2015.symbol.wellknown.d.ts%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A111%2C%22character%22%3A0%7D%5D "../../../../../Applications/Visual Studio Code.app/Contents/Resources/app/extensions/node_modules/typescript/lib/lib.es2015.symbol.wellknown.d.ts")): The timestamp when the tenure was selected. Defaults to the current time in ISO format.

##### Returns
- [`Promise<any>`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22external%22%3A%22file%3A%2F%2F%2FApplications%2FVisual%2520Studio%2520Code.app%2FContents%2FResources%2Fapp%2Fextensions%2Fnode_modules%2Ftypescript%2Flib%2Flib.es2015.iterable.d.ts%22%2C%22path%22%3A%22%2FApplications%2FVisual%20Studio%20Code.app%2FContents%2FResources%2Fapp%2Fextensions%2Fnode_modules%2Ftypescript%2Flib%2Flib.es2015.iterable.d.ts%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A215%2C%22character%22%3A0%7D%5D "../../../../../Applications/Visual Studio Code.app/Contents/Resources/app/extensions/node_modules/typescript/lib/lib.es2015.iterable.d.ts"): A promise that resolves to the response from the API, parsed as JSON.

##### Pseudo Logic
1. **Initialize Function**: Define the function [`setTenureApi`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FrispApis%2FsetTenureApi.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FrispApis%2FsetTenureApi.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FrispApis%2FsetTenureApi.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A0%2C%22character%22%3A0%7D%5D "src/apis/rispApis/setTenureApi.tsx") with the specified parameters.
2. **Construct Request Payload**: Create a JSON object containing the provided parameters.
3. **Send POST Request**:
   - Use the [`fetch`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22path%22%3A%22%2FApplications%2FVisual%20Studio%20Code.app%2FContents%2FResources%2Fapp%2Fextensions%2Fnode_modules%2Ftypescript%2Flib%2Flib.dom.d.ts%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A28326%2C%22character%22%3A0%7D%5D "../../../../../Applications/Visual Studio Code.app/Contents/Resources/app/extensions/node_modules/typescript/lib/lib.dom.d.ts") API to send a POST request to the specified endpoint.
   - Set the request method to `POST`.
   - Set the [`Content-Type`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FrispApis%2FsetTenureApi.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FrispApis%2FsetTenureApi.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FrispApis%2FsetTenureApi.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A15%2C%22character%22%3A10%7D%5D "src/apis/rispApis/setTenureApi.tsx") header to `application/json`.
   - Include the JSON payload in the request body.
4. **Handle Response**:
   - Check if the response is not OK (status code not in the range 200-299).
   - If the response is not OK, throw an error.
   - If the response is OK, parse the response as JSON and return it.
5. **Error Handling**:
   - Catch any errors that occur during the fetch operation.
   - Log the error to the console.
   - Rethrow the error to be handled by the caller.

##### Example Usage
```tsx
setTenure

Api

("vm123", "VirtualMachine1", "sub123", "us-east-1", "tenure1")
  .then(response => {
    console.log("API Response:", response);
  })
  .catch(error => {
    console.error("API Error:", error);
  });
```

##### Code
```tsx
export const setTenureApi = async (
  vm_id?: string,
  vm_name?: string,
  subscription_id?: string,
  location?: string,
  tenure?: string,
  selected_by: string = "User1.5",
  selected_at: string = new Date().toISOString() // Default to current time
): Promise<any> => {
  try {
    const response = await fetch(
      "https://n1ewphxcxc.execute-api.us-east-1.amazonaws.com/dev/tenure-data",
      {
        method: "POST",
        headers: {
          "Content-Type": "application/json",
        },
        body: JSON.stringify({
          vm_id,
          vm_name,
          location,
          subscription_id,
          selected_tenure: {
            tenure,
            selected_by,
            selected_at,
          },
        }),
      }
    );

    if (!response.ok) {
      throw new Error("Network response was not ok");
    }

    return await response.json();
  } catch (error) {
    console.error("Error sending data to API:", error);
    throw error;
  }
};
```

sumitchohan: generate detailed documentation for current code file including all methods, fields, properties and summary of their implementation including pseudo logic

GitHub Copilot: To generate detailed documentation for the provided code file [`accountAPI.tsx`](command:_github.copilot.openRelativePath?%5B%7B%22scheme%22%3A%22file%22%2C%22authority%22%3A%22%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FaccountAPI.tsx%22%2C%22query%22%3A%22%22%2C%22fragment%22%3A%22%22%7D%5D "/Users/sumit/Downloads/Frontend/NAGISVPR/src/apis/accountAPI.tsx"), we need to analyze the imports, constants, and any functions or methods defined in the file. Since the provided excerpt does not include any functions or methods, we will focus on documenting the imports and constants.

### Documentation for [`accountAPI.tsx`](command:_github.copilot.openRelativePath?%5B%7B%22scheme%22%3A%22file%22%2C%22authority%22%3A%22%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FaccountAPI.tsx%22%2C%22query%22%3A%22%22%2C%22fragment%22%3A%22%22%7D%5D "/Users/sumit/Downloads/Frontend/NAGISVPR/src/apis/accountAPI.tsx")

#### Summary
This file is part of the account management module and is responsible for handling account-related data and interactions with external services. It imports necessary modules and defines constants for account data.

#### Imports
- [`axios`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FaccountAPI.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FaccountAPI.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FaccountAPI.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A0%2C%22character%22%3A0%7D%5D "src/apis/accountAPI.tsx"): A promise-based HTTP client for making requests to external APIs.
- [`Account`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Futils%2Finterfaces.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A81%2C%22character%22%3A0%7D%5D "src/utils/interfaces.tsx"): An interface defining the structure of an account object.
- [`getConfiguration`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FclientAPI.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A10%2C%22character%22%3A0%7D%5D "src/apis/clientAPI.tsx"): A function from [`clientAPI`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FclientAPI.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A0%2C%22character%22%3A0%7D%5D "src/apis/clientAPI.tsx") for retrieving configuration settings.
- [`getRegistedUser`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fservices%2FauthService.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A20%2C%22character%22%3A0%7D%5D "src/services/authService.tsx"): A function from [`authService`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fservices%2FauthService.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A0%2C%22character%22%3A0%7D%5D "src/services/authService.tsx") for retrieving the registered user information.

#### Constants
- `acc_data`: A commented-out array of account objects. This array contains mock data for accounts from different cloud providers (Azure and AWS). Each account object includes various properties such as `UniqueId`, `CloudProvider`, `Alias`, `TenantId`, `SubscriptionId`, `SubscriptionName`, `Status`, `CreatedOn`, `AccountId`, `AccountName`, `ExternalId`, and `RoleARN`.

### Pseudo Logic
1. **Imports**:
   - Import necessary modules and interfaces.
   - [`axios`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FaccountAPI.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FaccountAPI.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FaccountAPI.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A0%2C%22character%22%3A0%7D%5D "src/apis/accountAPI.tsx") for making HTTP requests.
   - [`Account`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Futils%2Finterfaces.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A81%2C%22character%22%3A0%7D%5D "src/utils/interfaces.tsx") interface for defining account structure.
   - [`getConfiguration`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FclientAPI.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A10%2C%22character%22%3A0%7D%5D "src/apis/clientAPI.tsx") for retrieving configuration settings.
   - [`getRegistedUser`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fservices%2FauthService.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A20%2C%22character%22%3A0%7D%5D "src/services/authService.tsx") for retrieving registered user information.

2. **Constants**:
   - Define `acc_data` as an array of account objects (currently commented out).
   - Each account object contains properties such as:
     - `UniqueId`: A unique identifier for the account.
     - `CloudProvider`: The cloud provider (e.g., Azure, AWS).
     - `Alias`: An alias for the account.
     - `TenantId`: The tenant ID for Azure accounts.
     - `SubscriptionId`: The subscription ID for Azure accounts.
     - `SubscriptionName`: The subscription name for Azure accounts.
     - `Status`: The status of the account (e.g., Active).
     - `CreatedOn`: The creation date of the account.
     - `AccountId`: The account ID for AWS accounts.
     - `AccountName`: The account name for AWS accounts.
     - `ExternalId`: The external ID for AWS accounts.
     - `RoleARN`: The role ARN for AWS accounts.

### Example Documentation in Markdown

```markdown
# accountAPI.tsx

## Summary
This file is part of the account management module and is responsible for handling account-related data and interactions with external services. It imports necessary modules and defines constants for account data.

## Imports
- `axios`: A promise-based HTTP client for making requests to external APIs.
- `Account`: An interface defining the structure of an account object.
- `getConfiguration`: A function from `clientAPI` for retrieving configuration settings.
- `getRegistedUser`: A function from `authService` for retrieving the registered user information.

## Constants
### `acc_data`
A commented-out array of account objects. This array contains mock data for accounts from different cloud providers (Azure and AWS). Each account object includes various properties:

- **Azure Account Object**:
  - `UniqueId`: `"ncp_1724387999021"`
  - `CloudProvider`: `"Azure"`
  - `Alias`: `"Nagarro_Sigma_BU_Industry_Automation"`
  - `TenantId`: `"a45fe71a-f480-4e42-ad5e-aff33165aa35"`
  - `SubscriptionId`: `"03ee7d84-e877-43b4-a734-aac0253197be"`
  - `SubscriptionName`: `"Microsoft Azure Enterprise"`
  - `Status`: `"Active"`
  - `CreatedOn`: `"2024-08-23 04:39:59"`

- **AWS Account Object**

:


  - `AccountId`: `"411786980514"`
  - `AccountName`: `"isvpractice"`
  - `Alias`: `"ncp_dev_omendra"`
  - `CloudProvider`: `"AWS"`
  - `CreatedOn`: `"2024-05-10 11:05:58"`
  - `ExternalId`: `"6dd1f90d3d12d491ed516a8b2fe31609c708c334a504ec9410fa6f07d3e6634f"`
  - `RoleARN`: `"arn:aws:iam::411786980514:role/nagarro-cloud-pulse-role-20240510163558-CrossAccountRole"`
  - `Status`: `"Active"`
  - `UniqueId`: `"ncp_1715339157950"`

  - `AccountId`: `"411786980515"`
  - `AccountName`: `"isvpractice"`
  - `Alias`: `"ncp_dev_omendra_2"`
  - `CloudProvider`: `"AWS"`
  - `CreatedOn`: `"2024-05-10 11:05:58"`
```

This documentation provides a detailed overview of the current state of the `accountAPI.tsx` file, including its imports and constants.

sumitchohan: generate detailed documentation for current code file including all methods, fields, properties and summary of their implementation including pseudo logic


GitHub Copilot: ### AddRegionAPI.tsx Documentation

#### Summary
This file contains the implementation of an API call to add a region to an account. The main function `AddRegionApi` is responsible for sending a POST request to a specified URL with the account details and region information. The file also includes a data structure to hold the account and region information.

#### Fields and Properties

1. **data (Object)**
   - **AccountId (string)**: The ID of the account. Default value is `"411786980514"`.
   - **Regions (Array of strings)**: A list of region codes. Default values are `["us-east-1", "ap-south-1", "us-west-2", "ap-southeast-1"]`.
   - **EnvironmentName (string)**: The name of the environment. Default value is an empty string.
   - **Favourite (boolean)**: This field is commented out and can be included if needed.

#### Methods

1. **AddRegionApi (async function)**
   - **Parameters**:
     - `accountIdVar` (string): The account ID to be set in the data object.
     - `regionVar` (string): The region to be added to the data object.
     - `environmentNameVar` (string): The environment name to be set in the data object.
     - `favouriteVar` (boolean, optional): The favourite status to be set in the data object (commented out).
   - **Returns**: A promise that resolves to the response data from the API call or an empty array in case of an error.
   - **Pseudo Logic**:
     1. Define the URL for the API endpoint.
     2. Update the [`data`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FAddRegionAPI.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FAddRegionAPI.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FAddRegionAPI.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A34%2C%22character%22%3A0%7D%5D "src/apis/AddRegionAPI.tsx") object with the provided parameters.
     3. Log the [`data`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FAddRegionAPI.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FAddRegionAPI.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FAddRegionAPI.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A34%2C%22character%22%3A0%7D%5D "src/apis/AddRegionAPI.tsx") object to the console.
     4. Try to send a POST request to the API endpoint with the [`data`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FAddRegionAPI.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FAddRegionAPI.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FAddRegionAPI.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A34%2C%22character%22%3A0%7D%5D "src/apis/AddRegionAPI.tsx") object.
     5. Log the response to the console.
     6. Return the response data.
     7. Catch any errors, log them to the console, and return an empty array.

#### Pseudo Code

```plaintext
1. Define the URL for the API endpoint.
2. Update the `data` object with the provided parameters:
   - Set `data.AccountId` to `accountIdVar`.
   - Set `data.Region` to `regionVar`.
   - Set `data.EnvironmentName` to `environmentNameVar`.
   - Optionally set `data.Favourite` to `favouriteVar` (commented out).
3. Log the `data` object to the console.
4. Try to send a POST request to the API endpoint with the `data` object:
   - Await the response from the API.
   - Log the response to the console.
   - Return the response data.
5. Catch any errors:
   - Log the error to the console.
   - Return an empty array.
```

#### Code

```tsx
import axios from "axios";
import { API_BASE_URL } from "../utils/constants";

// Uncomment the favourite part if Favourite will added.

let data = {
  AccountId: "411786980514",
  Regions: ["us-east-1", "ap-south-1", "us-west-2", "ap-southeast-1"],
  // "Favourite": true
  EnvironmentName: "",
};

export const AddRegionApi = async (accountIdVar, regionVar, environmentNameVar, favouriteVar?): Promise<any> => {
  const url = "https://a31xxaetj8.execute-api.us-east-1.amazonaws.com/Dev/update/region";
  data.AccountId = accountIdVar;
  data.Region = regionVar;
  data.EnvironmentName = environmentNameVar;
  // data.Favourite = favouriteVar;
  console.log(data);
  try {
    const response = await axios.post(url, data);
    console.log(response);
    return response?.data;
  } catch (error) {
    console.log(error);
    return [];
  }
};
```

This documentation provides a detailed overview of the [`AddRegionAPI.tsx`](command:_github.copilot.openRelativePath?%5B%7B%22scheme%22%3A%22file%22%2C%22authority%22%3A%22%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FAddRegionAPI.tsx%22%2C%22query%22%3A%22%22%2C%22fragment%22%3A%22%22%7D%5D "/Users/sumit/Downloads/Frontend/NAGISVPR/src/apis/AddRegionAPI.tsx") file, including the fields, properties, and the main function's implementation logic.

sumitchohan: generate detailed documentation for current code file including all methods, fields, properties and summary of their implementation including pseudo logic

GitHub Copilot: ### azureAccountCreation.tsx Documentation

#### Summary
This file contains the implementation of an API call to create an Azure account. The main function [`azureCreateAccount`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FazureAccountCreation.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FazureAccountCreation.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FazureAccountCreation.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A8%2C%22character%22%3A0%7D%5D "src/apis/azureAccountCreation.tsx") is responsible for sending a POST request to a specified URL with the account alias and template string. The file also includes a data structure to hold the alias and template string information.

#### Fields and Properties

1. **data (Object)**
   - **alias (string)**: The alias for the Azure account. Default value is `"*"`.
   - **json_string (string)**: The JSON template string for the Azure account creation. Default value is `"*"`.

#### Methods

1. **azureCreateAccount (async function)**
   - **Parameters**:
     - [`template_string`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FazureAccountCreation.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FazureAccountCreation.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FazureAccountCreation.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A8%2C%22character%22%3A41%7D%5D "src/apis/azureAccountCreation.tsx") (string): The JSON template string for the Azure account creation.
     - [`alias`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FazureAccountCreation.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FazureAccountCreation.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FazureAccountCreation.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A4%2C%22character%22%3A2%7D%5D "src/apis/azureAccountCreation.tsx") (string): The alias for the Azure account.
   - **Returns**: A promise that resolves to the response data from the API call or an empty array in case of an error.
   - **Pseudo Logic**:
     1. Retrieve the configuration using [`getConfiguration`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FclientAPI.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A10%2C%22character%22%3A0%7D%5D "src/apis/clientAPI.tsx").
     2. Extract the URL for the API endpoint from the configuration.
     3. Update the [`data`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FazureAccountCreation.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FazureAccountCreation.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FazureAccountCreation.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A3%2C%22character%22%3A0%7D%5D "src/apis/azureAccountCreation.tsx") object with the provided parameters.
     4. Log the [`template_string`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FazureAccountCreation.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FazureAccountCreation.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FazureAccountCreation.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A8%2C%22character%22%3A41%7D%5D "src/apis/azureAccountCreation.tsx") to the console.
     5. Try to send a POST request to the API endpoint with the [`data`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FazureAccountCreation.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FazureAccountCreation.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FazureAccountCreation.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A3%2C%22character%22%3A0%7D%5D "src/apis/azureAccountCreation.tsx") object.
     6. Log the [`data`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FazureAccountCreation.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FazureAccountCreation.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FazureAccountCreation.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A3%2C%22character%22%3A0%7D%5D "src/apis/azureAccountCreation.tsx") object to the console.
     7. Log the response to the console.
     8. Return the response data.
     9. Catch any errors, log them to the console, and return an empty array.

#### Pseudo Code

```plaintext
1. Retrieve the configuration using `getConfiguration`.
2. Extract the URL for the API endpoint from the configuration.
3. Update the `data` object with the provided parameters:
   - Set `data.alias` to `alias`.
   - Set `data.json_string` to `template_string`.
4. Log the `template_string` to the console.
5. Try to send a POST request to the API endpoint with the `data` object:
   - Await the response from the API.
   - Log the response to the console.
   - Return the response data.
6. Catch any errors:
   - Log the error to the console.
   - Return an empty array.
```

#### Code

```tsx
import axios from "axios";
import { getConfiguration } from "./clientAPI";

let data = {
  alias: "*",
  json_string: "*",
};

export const azureCreateAccount = async (template_string: string, alias) => {
  //const url = "https://5cpadh8otj.execute-api.us-east-1.amazonaws.com/development/azure/createaccount";
  const configuration = await getConfiguration();
  const url = configuration.api.cloudpulse_create_azure_account;
  data.alias = alias;
  data.json_string = template_string;
  console.log(template

_string

);

  try {
    console.log(data);
    const response = await axios.post(url, data);
    // console.log(response.data.body.data.UniqueId);
    console.log(response);
    return response.data;
  } catch (error) {
    console.log(error);
    return [];
  }
};
```

This documentation provides a detailed overview of the [`azureAccountCreation.tsx`](command:_github.copilot.openRelativePath?%5B%7B%22scheme%22%3A%22file%22%2C%22authority%22%3A%22%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FazureAccountCreation.tsx%22%2C%22query%22%3A%22%22%2C%22fragment%22%3A%22%22%7D%5D "/Users/sumit/Downloads/Frontend/NAGISVPR/src/apis/azureAccountCreation.tsx") file, including the fields, properties, and the main function's implementation logic.

sumitchohan: generate detailed documentation for current code file including all methods, fields, properties and summary of their implementation including pseudo logic

GitHub Copilot: ### Documentation for [`clientAPI.tsx`](command:_github.copilot.openRelativePath?%5B%7B%22scheme%22%3A%22file%22%2C%22authority%22%3A%22%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FclientAPI.tsx%22%2C%22query%22%3A%22%22%2C%22fragment%22%3A%22%22%7D%5D "/Users/sumit/Downloads/Frontend/NAGISVPR/src/apis/clientAPI.tsx")

#### Summary
This file contains various API-related functions for fetching and managing account data, configurations, and date lists. It uses [`axios`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FclientAPI.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FclientAPI.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FclientAPI.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A0%2C%22character%22%3A0%7D%5D "src/apis/clientAPI.tsx") for HTTP requests and includes error handling and logging.

#### Methods

1. **getAccountList**
   - **Description**: Returns a static list of account names.
   - **Returns**: [`Array<string>`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22external%22%3A%22file%3A%2F%2F%2FApplications%2FVisual%2520Studio%2520Code.app%2FContents%2FResources%2Fapp%2Fextensions%2Fnode_modules%2Ftypescript%2Flib%2Flib.es2015.core.d.ts%22%2C%22path%22%3A%22%2FApplications%2FVisual%20Studio%20Code.app%2FContents%2FResources%2Fapp%2Fextensions%2Fnode_modules%2Ftypescript%2Flib%2Flib.es2015.core.d.ts%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A18%2C%22character%22%3A0%7D%5D "../../../../../Applications/Visual Studio Code.app/Contents/Resources/app/extensions/node_modules/typescript/lib/lib.es2015.core.d.ts") - A list of account names.
   - **Pseudo Logic**:
     - Return a hardcoded array of account names.

2. **getConfiguration**
   - **Description**: Fetches configuration data from a local JSON file.
   - **Returns**: [`Promise<any>`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22external%22%3A%22file%3A%2F%2F%2FApplications%2FVisual%2520Studio%2520Code.app%2FContents%2FResources%2Fapp%2Fextensions%2Fnode_modules%2Ftypescript%2Flib%2Flib.es2015.iterable.d.ts%22%2C%22path%22%3A%22%2FApplications%2FVisual%20Studio%20Code.app%2FContents%2FResources%2Fapp%2Fextensions%2Fnode_modules%2Ftypescript%2Flib%2Flib.es2015.iterable.d.ts%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A215%2C%22character%22%3A0%7D%5D "../../../../../Applications/Visual Studio Code.app/Contents/Resources/app/extensions/node_modules/typescript/lib/lib.es2015.iterable.d.ts") - The configuration data parsed from the JSON file, or `null` if an error occurs.
   - **Pseudo Logic**:
     - Try to fetch [`configuration.json`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FclientAPI.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FclientAPI.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FclientAPI.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A25%2C%22character%22%3A2%7D%5D "src/apis/clientAPI.tsx") using a GET request.
     - If the response is OK, parse and return the JSON data.
     - If an error occurs, log the error and return `null`.

3. **getAllAccounts**
   - **Description**: Fetches a list of all accounts from an external API.
   - **Returns**: [`Promise<Array<Account>>`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22external%22%3A%22file%3A%2F%2F%2FApplications%2FVisual%2520Studio%2520Code.app%2FContents%2FResources%2Fapp%2Fextensions%2Fnode_modules%2Ftypescript%2Flib%2Flib.es2015.iterable.d.ts%22%2C%22path%22%3A%22%2FApplications%2FVisual%20Studio%20Code.app%2FContents%2FResources%2Fapp%2Fextensions%2Fnode_modules%2Ftypescript%2Flib%2Flib.es2015.iterable.d.ts%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A215%2C%22character%22%3A0%7D%5D "../../../../../Applications/Visual Studio Code.app/Contents/Resources/app/extensions/node_modules/typescript/lib/lib.es2015.iterable.d.ts") - A list of accounts, or an empty array if an error occurs.
   - **Pseudo Logic**:
     - Call [`getConfiguration`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FclientAPI.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FclientAPI.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FclientAPI.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A10%2C%22character%22%3A0%7D%5D "src/apis/clientAPI.tsx") to get the configuration data.
     - Extract the API URL for fetching all accounts from the configuration.
     - Try to fetch the account data using [`axios.get`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FclientAPI.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FclientAPI.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FclientAPI.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A0%2C%22character%22%3A0%7D%5D "src/apis/clientAPI.tsx").
     - If successful, return the data from the response.
     - If an error occurs, log the error and return an empty array.

4. **getDateList**
   - **Description**: Returns a static list of date ranges.
   - **Returns**: [`Array<string>`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22external%22%3A%22file%3A%2F%2F%2FApplications%2FVisual%2520Studio%2520Code.app%2FContents%2FResources%2Fapp%2Fextensions%2Fnode_modules%2Ftypescript%2Flib%2Flib.es2015.core.d.ts%22%2C%22path%22%3A%22%2FApplications%2FVisual%20Studio%20Code.app%2FContents%2FResources%2Fapp%2Fextensions%2Fnode_modules%2Ftypescript%2Flib%2Flib.es2015.core.d.ts%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A18%2C%22character%22%3A0%7D%5D "../../../../../Applications/Visual Studio Code.app/Contents/Resources/app/extensions/node_modules/typescript/lib/lib.es2015.core.d.ts") - A list of date ranges.
   - **Pseudo Logic**:
     - Return a hardcoded array of date ranges.

### Code Documentation

```tsx
/**
 * Fetches a static list of account names.
 * 
 * @returns {Array<string>} A list of account names.
 */
export const getAccountList = (): Array<string> => {
  return ["CloudPulse DEV", "CloudPulse QA", "CloudPulse PROD"];
};

/**
 * Fetches configuration data from a local JSON file.
 * 
 * @returns {Promise<any>} The configuration data parsed from the JSON file, or `null` if an error occurs.
 */
export const getConfiguration = async () => {
  try {
    const response = await fetch(`configuration.json`, {
      method: "GET",
    });
    if (response.ok) {
      return response.json();
    }
  } catch (error) {
    console.log(error);
  }
  return null;
};

/**
 * Fetches a list of all accounts from an external API.
 * 
 * @returns {Promise<Array<Account>>} A list of accounts, or an empty array if an error occurs.
 */
export const getAllAccounts = async (): Promise<Array<Account>> => {
  const configuration = await getConfiguration();
  const url = configuration.api.cloudpulse_all_account;
  const body = {};
  try {
    const response = await axios.get(url, body);
    return response?.data.body;
  } catch (error) {
    console.log(error);
    return [] as Array<Account>;
  }
};

/**
 * Fetches a static list of date ranges.
 * 
 * @returns {Array<string>} A list of date ranges.
 */
export const getDateList = () => {
  return ["Last Month", "Last 2 Months", "Last 6 months", "Last 12 months"];
};
```

### Summary of Implementation

- **getAccountList**: Returns a predefined list of account names.
- **getConfiguration**: Fetches configuration data from a local JSON file and handles errors by logging them.
- **getAllAccounts**: Uses the configuration data to fetch a list of accounts from an external API, with error handling.
- **getDateList**: Returns a predefined list of date ranges.

sumitchohan: generate detailed documentation for current code file including all methods, fields, properties and summary of their implementation including pseudo logic

GitHub Copilot: ### Documentation for [`createAccount.tsx`](command:_github.copilot.openRelativePath?%5B%7B%22scheme%22%3A%22file%22%2C%22authority%22%3A%22%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FcreateAccount.tsx%22%2C%22query%22%3A%22%22%2C%22fragment%22%3A%22%22%7D%5D "/Users/sumit/Downloads/Frontend/NAGISVPR/src/apis/createAccount.tsx")

#### Summary
This file contains functions to fetch configuration data and create an account using an API. It uses [`axios`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FcreateAccount.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FcreateAccount.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FcreateAccount.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A0%2C%22character%22%3A0%7D%5D "src/apis/createAccount.tsx") for making HTTP requests and handles asynchronous operations with `async/await`.

#### Fields
- [`data`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FcreateAccount.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FcreateAccount.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FcreateAccount.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A2%2C%22character%22%3A0%7D%5D "src/apis/createAccount.tsx"): An object that holds the default values for [`CloudProvider`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FcreateAccount.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FcreateAccount.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FcreateAccount.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A3%2C%22character%22%3A2%7D%5D "src/apis/createAccount.tsx"), [`DefaultRegion`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FcreateAccount.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FcreateAccount.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FcreateAccount.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A4%2C%22character%22%3A2%7D%5D "src/apis/createAccount.tsx"), and [`Alias`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FcreateAccount.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FcreateAccount.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FcreateAccount.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A5%2C%22character%22%3A2%7D%5D "src/apis/createAccount.tsx").

```typescript
let data = {
  CloudProvider: "*",
  DefaultRegion: "*",
  Alias: "*",
};
```

#### Methods

1. **getConfiguration**
   - **Description**: Fetches configuration data from a `configuration.json` file.
   - **Parameters**: None
   - **Returns**: A promise that resolves to the configuration data in JSON format or `null` if an error occurs.
   - **Pseudo Logic**:
     1. Attempt to fetch `configuration.json` using a GET request.
     2. If the response is successful, return the JSON data.
     3. If an error occurs, log the error and return `null`.

```typescript
export const getConfiguration = async () => {
  try {
    const response = await fetch([`configuration.json`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FcreateAccount.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FcreateAccount.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FcreateAccount.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A27%2C%22character%22%3A2%7D%5D "src/apis/createAccount.tsx"), {
      method: "GET",
    });
    if (response.ok) {
      return response.json();
    }
  } catch (error) {
    console.log(error);
  }
  return null;
};
```

2. **createAccountAPI**
   - **Description**: Creates an account by sending a POST request to the API endpoint specified in the configuration.
   - **Parameters**:
     - [`cloudProviderVar`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FcreateAccount.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FcreateAccount.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FcreateAccount.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A23%2C%22character%22%3A2%7D%5D "src/apis/createAccount.tsx"): The cloud provider to be set in the [`data`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FcreateAccount.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FcreateAccount.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FcreateAccount.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A2%2C%22character%22%3A0%7D%5D "src/apis/createAccount.tsx") object.
     - [`defaultRegionVar`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FcreateAccount.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FcreateAccount.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FcreateAccount.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A24%2C%22character%22%3A2%7D%5D "src/apis/createAccount.tsx"): The default region to be set in the [`data`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FcreateAccount.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FcreateAccount.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FcreateAccount.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A2%2C%22character%22%3A0%7D%5D "src/apis/createAccount.tsx") object.
     - [`aliasDataVar`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FcreateAccount.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FcreateAccount.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FcreateAccount.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A25%2C%22character%22%3A2%7D%5D "src/apis/createAccount.tsx"): The alias to be set in the [`data`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FcreateAccount.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FcreateAccount.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FcreateAccount.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A2%2C%22character%22%3A0%7D%5D "src/apis/createAccount.tsx") object.
   - **Returns**: A promise that resolves to the response data from the API or logs an error if the request fails.
   - **Pseudo Logic**:
     1. Fetch the configuration using [`getConfiguration`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FcreateAccount.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FcreateAccount.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FcreateAccount.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A8%2C%22character%22%3A0%7D%5D "src/apis/createAccount.tsx").
     2. Extract the API URL from the configuration.
     3. Update the [`data`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FcreateAccount.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FcreateAccount.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FcreateAccount.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A2%2C%22character%22%3A0%7D%5D "src/apis/createAccount.tsx") object with the provided parameters.
     4. Attempt to send a POST request to the API with the [`data`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FcreateAccount.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FcreateAccount.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FcreateAccount.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A2%2C%22character%22%3A0%7D%5D "src/apis/createAccount.tsx") object.
     5. Log and return the response data if the request is successful.
     6. If an error occurs, log the error.

```typescript
export const createAccountAPI = async (
  cloudProviderVar,
  defaultRegionVar,
  aliasDataVar
): Promise<any> => {
  const configuration = await getConfiguration();
  const url = configuration.api.cloudpulse_create_account;
  data.CloudProvider = cloudProviderVar;
  data.DefaultRegion = defaultRegionVar;
  data.Alias = aliasDataVar;
  try {
    console.log(data);
    const response = await axios.post(url, data);
    console.log(response?.data.body.data);
    return response?.data.body.data;
  } catch (error) {
    console.log(error);
  }
};
```

### Notes
- The `createAccountAPI` function relies on the `getConfiguration` function to fetch the API endpoint.
- Error handling is done using `try/catch` blocks, and errors are logged to the console.
- The `data` object is updated with the parameters passed to `createAccountAPI` before making the POST request.

sumitchohan: generate detailed documentation for current code file including all methods, fields, properties and summary of their implementation including pseudo logic

GitHub Copilot: ### File: [`createAccountCheckStack.tsx`](command:_github.copilot.openRelativePath?%5B%7B%22scheme%22%3A%22file%22%2C%22authority%22%3A%22%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FcreateAccountCheckStack.tsx%22%2C%22query%22%3A%22%22%2C%22fragment%22%3A%22%22%7D%5D "/Users/sumit/Downloads/Frontend/NAGISVPR/src/apis/createAccountCheckStack.tsx")

#### Summary
This file contains two asynchronous functions: [`getConfiguration`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FcreateAccountCheckStack.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FcreateAccountCheckStack.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FcreateAccountCheckStack.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A6%2C%22character%22%3A0%7D%5D "src/apis/createAccountCheckStack.tsx") and [`createAccountStackCheck`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FcreateAccountCheckStack.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FcreateAccountCheckStack.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FcreateAccountCheckStack.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A20%2C%22character%22%3A0%7D%5D "src/apis/createAccountCheckStack.tsx"). These functions are used to fetch configuration data from a JSON file and to make a POST request to a specified URL with a unique identifier.

#### Fields
- [`data`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FcreateAccountCheckStack.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FcreateAccountCheckStack.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FcreateAccountCheckStack.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A2%2C%22character%22%3A0%7D%5D "src/apis/createAccountCheckStack.tsx"): An object with a single property [`UniqueId`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FcreateAccountCheckStack.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FcreateAccountCheckStack.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FcreateAccountCheckStack.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A3%2C%22character%22%3A2%7D%5D "src/apis/createAccountCheckStack.tsx") initialized to `"*"`.

#### Functions

1. **getConfiguration**
   - **Description**: Fetches configuration data from a [`configuration.json`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FcreateAccountCheckStack.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FcreateAccountCheckStack.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FcreateAccountCheckStack.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A23%2C%22character%22%3A2%7D%5D "src/apis/createAccountCheckStack.tsx") file.
   - **Parameters**: None
   - **Returns**: A promise that resolves to the JSON content of the configuration file if the fetch is successful, otherwise `null`.
   - **Pseudo Logic**:
     1. Try to fetch the [`configuration.json`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FcreateAccountCheckStack.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FcreateAccountCheckStack.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FcreateAccountCheckStack.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A23%2C%22character%22%3A2%7D%5D "src/apis/createAccountCheckStack.tsx") file using a GET request.
     2. If the response is OK, return the JSON content of the response.
     3. If an error occurs, log the error and return `null`.

2. **createAccountStackCheck**
   - **Description**: Sends a POST request to a specified URL with a unique identifier.
   - **Parameters**:
     - [`UniqueIdVar`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FcreateAccountCheckStack.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FcreateAccountCheckStack.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FcreateAccountCheckStack.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A21%2C%22character%22%3A2%7D%5D "src/apis/createAccountCheckStack.tsx") (string): The unique identifier to be sent in the POST request.
   - **Returns**: A promise that resolves to the response data if the POST request is successful, otherwise an empty array.
   - **Pseudo Logic**:
     1. Call [`getConfiguration`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FcreateAccountCheckStack.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FcreateAccountCheckStack.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FcreateAccountCheckStack.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A6%2C%22character%22%3A0%7D%5D "src/apis/createAccountCheckStack.tsx") to fetch the configuration data.
     2. Extract the URL for the API endpoint from the configuration data.
     3. Update the [`UniqueId`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FcreateAccountCheckStack.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FcreateAccountCheckStack.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FcreateAccountCheckStack.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A3%2C%22character%22%3A2%7D%5D "src/apis/createAccountCheckStack.tsx") property of the [`data`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FcreateAccountCheckStack.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FcreateAccountCheckStack.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FcreateAccountCheckStack.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A2%2C%22character%22%3A0%7D%5D "src/apis/createAccountCheckStack.tsx") object with [`UniqueIdVar`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FcreateAccountCheckStack.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FcreateAccountCheckStack.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FcreateAccountCheckStack.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A21%2C%22character%22%3A2%7D%5D "src/apis/createAccountCheckStack.tsx").
     4. Try to send a POST request to the extracted URL with the [`data`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FcreateAccountCheckStack.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FcreateAccountCheckStack.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FcreateAccountCheckStack.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A2%2C%22character%22%3A0%7D%5D "src/apis/createAccountCheckStack.tsx") object.
     5. Log the response and return the response data if the request is successful.
     6. If an error occurs, log the error and return an empty array.

### Code Documentation

```tsx
import axios from "axios";

// Data object with a single property UniqueId
let data = {
  UniqueId: "*",
};

/**
 * Fetches configuration data from a configuration.json file.
 * 
 * @returns {Promise<Object|null>} A promise that resolves to the JSON content of the configuration file if successful, otherwise null.
 */
export const getConfiguration = async () => {
  try {
    const response = await fetch(`configuration.json`, {
      method: "GET",
    });
    if (response.ok) {
      return response.json();
    }
  } catch (error) {
    console.log(error);
  }
  return null;
};

/**
 * Sends a POST request to a specified URL with a unique identifier.
 * 
 * @param {string} UniqueIdVar - The unique identifier to be sent in the POST request.
 * @returns {Promise<any>} A promise that resolves to the response data if successful, otherwise an empty array.
 */
export const createAccountStackCheck = async (
  UniqueIdVar: string
): Promise<any> => {
  const configuration = await getConfiguration();
  const url = configuration.api.cloudpulse_create_account_stack;
  // const url =
  //   "https://fmeu9t1e0a.execute-api.us-east-1.amazonaws.com/production/rolearn";
  data.UniqueId = UniqueIdVar;
  try {
    const response = await axios.post(url, data);
    console.log(response);
    return response?.data;
  } catch (error) {
    console.log(error);
    return [];
  }
};
```

sumitchohan: generate detailed documentation for current code file including all methods, fields, properties and summary of their implementation including pseudo logic

GitHub Copilot: ### Documentation for [`createStackAPI.tsx`](command:_github.copilot.openRelativePath?%5B%7B%22scheme%22%3A%22file%22%2C%22authority%22%3A%22%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FcreateStackAPI.tsx%22%2C%22query%22%3A%22%22%2C%22fragment%22%3A%22%22%7D%5D "/Users/sumit/Downloads/Frontend/NAGISVPR/src/apis/createStackAPI.tsx")

#### Summary
This file contains functions to interact with an API for creating a stack. It uses [`axios`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FcreateStackAPI.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FcreateStackAPI.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FcreateStackAPI.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A0%2C%22character%22%3A0%7D%5D "src/apis/createStackAPI.tsx") for HTTP requests and fetches configuration data from a local JSON file.

#### Fields
- [`data`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FcreateStackAPI.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FcreateStackAPI.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FcreateStackAPI.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A3%2C%22character%22%3A0%7D%5D "src/apis/createStackAPI.tsx"): An object containing various fields required for the API request.
  - [`ExternalId`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FcreateStackAPI.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FcreateStackAPI.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FcreateStackAPI.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A4%2C%22character%22%3A2%7D%5D "src/apis/createStackAPI.tsx"): Initially set to `"*"`.
  - [`AccountId`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FcreateStackAPI.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FcreateStackAPI.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FcreateStackAPI.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A5%2C%22character%22%3A2%7D%5D "src/apis/createStackAPI.tsx"): Set to `"411786980517"`.
  - [`AccountName`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FcreateStackAPI.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FcreateStackAPI.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FcreateStackAPI.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A6%2C%22character%22%3A2%7D%5D "src/apis/createStackAPI.tsx"): Initially an empty string.
  - [`RoleARN`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FcreateStackAPI.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FcreateStackAPI.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FcreateStackAPI.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A7%2C%22character%22%3A2%7D%5D "src/apis/createStackAPI.tsx"): Set to `"arn:aws:iam::202345678912:role/test-role"`.
  - [`LambdaRoleARN`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FcreateStackAPI.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FcreateStackAPI.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FcreateStackAPI.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A8%2C%22character%22%3A2%7D%5D "src/apis/createStackAPI.tsx"): Set to `"arn:aws:iam::411786980514:role/nagarro-cloud-pulse-role-20240301183000-LambdaExecutionRole"`.
  - [`LatestComment`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FcreateStackAPI.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FcreateStackAPI.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FcreateStackAPI.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A10%2C%22character%22%3A2%7D%5D "src/apis/createStackAPI.tsx"): Set to `"This is the latest comment for the update"`.

#### Methods

1. **[`getConfiguration`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FcreateStackAPI.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FcreateStackAPI.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FcreateStackAPI.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A13%2C%22character%22%3A0%7D%5D "src/apis/createStackAPI.tsx")**
   - **Description**: Fetches configuration data from a local JSON file.
   - **Parameters**: None
   - **Returns**: A promise that resolves to the configuration object if the fetch is successful, otherwise `null`.
   - **Pseudo Logic**:
     1. Try to fetch [`configuration.json`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FcreateStackAPI.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FcreateStackAPI.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FcreateStackAPI.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A28%2C%22character%22%3A2%7D%5D "src/apis/createStackAPI.tsx") using a GET request.
     2. If the response is OK, return the JSON data.
     3. If an error occurs, log the error and return `null`.

2. **[`createStackFuncAPI`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FcreateStackAPI.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FcreateStackAPI.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FcreateStackAPI.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A27%2C%22character%22%3A0%7D%5D "src/apis/createStackAPI.tsx")**
   - **Description**: Sends a POST request to create a stack using the provided [`external_id`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FcreateStackAPI.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FcreateStackAPI.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FcreateStackAPI.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A27%2C%22character%22%3A41%7D%5D "src/apis/createStackAPI.tsx").
   - **Parameters**:
     - [`external_id`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FcreateStackAPI.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FcreateStackAPI.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FcreateStackAPI.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A27%2C%22character%22%3A41%7D%5D "src/apis/createStackAPI.tsx") (string): The external ID to be used in the request.
   - **Returns**: A promise that resolves to the response data if the request is successful, otherwise an empty array.
   - **Pseudo Logic**:
     1. Call [`getConfiguration`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FcreateStackAPI.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FcreateStackAPI.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FcreateStackAPI.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A13%2C%22character%22%3A0%7D%5D "src/apis/createStackAPI.tsx") to fetch the configuration data.
     2. Extract the URL for the API endpoint from the configuration.
     3. Update the [`ExternalId`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FcreateStackAPI.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FcreateStackAPI.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FcreateStackAPI.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A4%2C%22character%22%3A2%7D%5D "src/apis/createStackAPI.tsx") field in the [`data`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FcreateStackAPI.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FcreateStackAPI.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FcreateStackAPI.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A3%2C%22character%22%3A0%7D%5D "src/apis/createStackAPI.tsx") object with the provided [`external_id`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FcreateStackAPI.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FcreateStackAPI.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FcreateStackAPI.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A27%2C%22character%22%3A41%7D%5D "src/apis/createStackAPI.tsx").
     4. Log the [`data`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FcreateStackAPI.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FcreateStackAPI.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FcreateStackAPI.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A3%2C%22character%22%3A0%7D%5D "src/apis/createStackAPI.tsx") object.
     5. Try to send a POST request to the extracted URL with the [`data`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FcreateStackAPI.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FcreateStackAPI.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FcreateStackAPI.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A3%2C%22character%22%3A0%7D%5D "src/apis/createStackAPI.tsx") object.
     6. If the request is successful, log and return the response data.
     7. If an error occurs, log the error and return an empty array.

### Code
```tsx
import axios from "axios";
import { API_BASE_URL } from "../utils/constants";

let data = {
  ExternalId: "*",
  AccountId: "411786980517",
  AccountName: "",
  RoleARN: "arn:aws:iam::202345678912:role/test-role",
  LambdaRoleARN:
    "arn:aws:iam::411786980514:role/nagarro-cloud-pulse-role-20240301183000-LambdaExecutionRole",
  LatestComment: "This is the latest comment for the update",
};

/**
 * Fetches configuration data from a local JSON file.
 * @returns {Promise<Object|null>} A promise that resolves to the configuration object if successful, otherwise null.
 */
export const getConfiguration = async () => {
  try {
    const response = await fetch(`configuration.json`, {
      method: "GET",
    });
    if (response.ok) {
      return response.json();
    }
  } catch (error) {
    console.log(error);
  }
  return null;
};

/**
 * Sends a POST request to create a stack using the provided external_id.
 * @param {string} external_id - The external ID to be used in the request.
 * @returns {Promise<any>} A promise that resolves to the response data if successful, otherwise an empty array.
 */
export const createStackFuncAPI = async (external_id: string): Promise<any> => {
  const configuration = await getConfiguration();
  const url = configuration.api.cloudpulse_create_stack;
  data.ExternalId = external_id;
  console.log(data);
  try {
    const response = await axios.post(url, data);
    console.log(response);
    return response?.data;
  } catch (error) {
    console.log(error);
    return [];
};
```

sumitchohan: generate detailed documentation for current code file including all methods, fields, properties and summary of their implementation including pseudo logic


GitHub Copilot: ### File: `dashboardAPI.tsx`

#### Summary
This file contains an API utility function to fetch policy compliance status for a given account and cloud provider. It uses [`axios`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FdasboardAPI.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FdasboardAPI.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FdasboardAPI.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A0%2C%22character%22%3A0%7D%5D "src/apis/dasboardAPI.tsx") for making HTTP requests and retrieves configuration details from another module.

#### Functions

1. **getPolicyComplianceStatus**

   - **Description**: This function fetches the policy compliance status for a specified account and cloud provider.
   - **Parameters**:
     - [`accountId`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FdasboardAPI.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FdasboardAPI.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FdasboardAPI.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A5%2C%22character%22%3A2%7D%5D "src/apis/dasboardAPI.tsx") (string): The ID of the account for which the policy compliance status is to be fetched.
     - [`cloudProvider`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FdasboardAPI.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FdasboardAPI.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FdasboardAPI.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A6%2C%22character%22%3A2%7D%5D "src/apis/dasboardAPI.tsx") (string): The cloud provider (e.g., AWS, Azure) for which the policy compliance status is to be fetched.
   - **Returns**: A [`Promise`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22external%22%3A%22file%3A%2F%2F%2FApplications%2FVisual%2520Studio%2520Code.app%2FContents%2FResources%2Fapp%2Fextensions%2Fnode_modules%2Ftypescript%2Flib%2Flib.es2015.iterable.d.ts%22%2C%22path%22%3A%22%2FApplications%2FVisual%20Studio%20Code.app%2FContents%2FResources%2Fapp%2Fextensions%2Fnode_modules%2Ftypescript%2Flib%2Flib.es2015.iterable.d.ts%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A215%2C%22character%22%3A0%7D%5D "../../../../../Applications/Visual Studio Code.app/Contents/Resources/app/extensions/node_modules/typescript/lib/lib.es2015.iterable.d.ts") that resolves to the policy compliance status data or an empty array in case of an error.
   - **Pseudo Logic**:
     1. Retrieve the configuration using [`getConfiguration()`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FclientAPI.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A10%2C%22character%22%3A0%7D%5D "src/apis/clientAPI.tsx").
     2. Extract the `cloudpulse_dashboard` URL from the configuration.
     3. Construct the request body with [`accountId`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FdasboardAPI.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FdasboardAPI.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FdasboardAPI.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A5%2C%22character%22%3A2%7D%5D "src/apis/dasboardAPI.tsx") and [`cloudProvider`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FdasboardAPI.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FdasboardAPI.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FdasboardAPI.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A6%2C%22character%22%3A2%7D%5D "src/apis/dasboardAPI.tsx").
     4. Log the request body for debugging purposes.
     5. Make a POST request to the extracted URL with the constructed body using [`axios`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FdasboardAPI.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FdasboardAPI.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FdasboardAPI.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A0%2C%22character%22%3A0%7D%5D "src/apis/dasboardAPI.tsx").
     6. Return the response data's body if the request is successful.
     7. Catch any errors, log them, and return an empty array.

#### Imports

- [`axios`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FdasboardAPI.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FdasboardAPI.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FdasboardAPI.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A0%2C%22character%22%3A0%7D%5D "src/apis/dasboardAPI.tsx"): A promise-based HTTP client for making requests.
- [`API_BASE_URL`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Futils%2Fconstants.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A5%2C%22character%22%3A0%7D%5D "src/utils/constants.tsx"): A constant that holds the base URL for the API (not used in the current function).
- [`getConfiguration`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FclientAPI.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A10%2C%22character%22%3A0%7D%5D "src/apis/clientAPI.tsx"): A function that retrieves configuration details, including API endpoints.

#### Code
```tsx
import axios from "axios";
import { API_BASE_URL } from "../utils/constants";
import { getConfiguration } from "./clientAPI";

/**
 * Fetches the policy compliance status for a given account and cloud provider.
 * 
 * @param {string} accountId - The ID of the account.
 * @param {string} cloudProvider - The cloud provider (e.g., AWS, Azure).
 * @returns {Promise<any>} - A promise that resolves to the policy compliance status data.
 */
export const getPolicyComplianceStatus = async (
  accountId: string,
  cloudProvider: string
): Promise<any> => {
  const configuration = await getConfiguration();
  const url = configuration.api.cloudpulse_dashboard;
  // const url =
  //   "https://ea01y6hvjk.execute-api.us-east-1.amazonaws.com/production/totalpolicies";
  try {
    const body = {
      AccountId: accountId,
      cloud_provider: cloudProvider,
    };
    console.log(body);
    var response = await axios.post(url, body);
    return response?.data.body;
  } catch (error) {
    console.log(error);
    return [];
  }
};
```

This documentation provides a detailed overview of the `dashboardAPI.tsx` file, including the purpose of the function, its parameters, return values, and a step-by-step explanation of its logic.

sumitchohan: generate detailed documentation for current code file including all methods, fields, properties and summary of their implementation including pseudo logic

GitHub Copilot: ### Documentation for [`deleteAccountAPI.tsx`](command:_github.copilot.openRelativePath?%5B%7B%22scheme%22%3A%22file%22%2C%22authority%22%3A%22%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FdeleteAccountAPI.tsx%22%2C%22query%22%3A%22%22%2C%22fragment%22%3A%22%22%7D%5D "/Users/sumit/Downloads/Frontend/NAGISVPR/src/apis/deleteAccountAPI.tsx")

#### Summary
This file contains two main functions: [`getConfiguration`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FdeleteAccountAPI.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FdeleteAccountAPI.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FdeleteAccountAPI.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A7%2C%22character%22%3A0%7D%5D "src/apis/deleteAccountAPI.tsx") and [`deleteAccount`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FdeleteAccountAPI.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FdeleteAccountAPI.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FdeleteAccountAPI.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A21%2C%22character%22%3A0%7D%5D "src/apis/deleteAccountAPI.tsx"). These functions are used to fetch configuration data and delete an account by making HTTP requests.

#### Fields
- [`data`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FdeleteAccountAPI.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FdeleteAccountAPI.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FdeleteAccountAPI.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A3%2C%22character%22%3A0%7D%5D "src/apis/deleteAccountAPI.tsx"): An object with a single property [`UniqueId`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FdeleteAccountAPI.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FdeleteAccountAPI.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FdeleteAccountAPI.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A4%2C%22character%22%3A2%7D%5D "src/apis/deleteAccountAPI.tsx") initialized to `"*"`. This object is used to store the account ID that needs to be deleted.

#### Methods

1. **getConfiguration**
   - **Description**: Fetches configuration data from a [`configuration.json`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FdeleteAccountAPI.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FdeleteAccountAPI.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FdeleteAccountAPI.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A22%2C%22character%22%3A2%7D%5D "src/apis/deleteAccountAPI.tsx") file.
   - **Parameters**: None
   - **Returns**: A promise that resolves to the configuration data in JSON format if the fetch is successful, otherwise `null`.
   - **Pseudo Logic**:
     1. Try to fetch the [`configuration.json`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FdeleteAccountAPI.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FdeleteAccountAPI.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FdeleteAccountAPI.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A22%2C%22character%22%3A2%7D%5D "src/apis/deleteAccountAPI.tsx") file using a GET request.
     2. If the response is successful ([`response.ok`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FdeleteAccountAPI.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FdeleteAccountAPI.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FdeleteAccountAPI.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A9%2C%22character%22%3A4%7D%5D "src/apis/deleteAccountAPI.tsx")), parse and return the JSON data.
     3. If an error occurs during the fetch, log the error and return `null`.

2. **deleteAccount**
   - **Description**: Deletes an account by making a DELETE request to a specified URL.
   - **Parameters**:
     - [`accountIdVar`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FdeleteAccountAPI.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FdeleteAccountAPI.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FdeleteAccountAPI.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A21%2C%22character%22%3A36%7D%5D "src/apis/deleteAccountAPI.tsx") (any): The unique ID of the account to be deleted.
   - **Returns**: A promise that resolves to the response data if the DELETE request is successful, otherwise an empty array.
   - **Pseudo Logic**:
     1. Call [`getConfiguration`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FdeleteAccountAPI.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FdeleteAccountAPI.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FdeleteAccountAPI.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A7%2C%22character%22%3A0%7D%5D "src/apis/deleteAccountAPI.tsx") to fetch the configuration data.
     2. Extract the `cloudpulse_delete_account` URL from the configuration data.
     3. Update the [`UniqueId`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FdeleteAccountAPI.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FdeleteAccountAPI.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FdeleteAccountAPI.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A4%2C%22character%22%3A2%7D%5D "src/apis/deleteAccountAPI.tsx") property of the [`data`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FdeleteAccountAPI.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FdeleteAccountAPI.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FdeleteAccountAPI.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A3%2C%22character%22%3A0%7D%5D "src/apis/deleteAccountAPI.tsx") object with [`accountIdVar`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FdeleteAccountAPI.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FdeleteAccountAPI.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FdeleteAccountAPI.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A21%2C%22character%22%3A36%7D%5D "src/apis/deleteAccountAPI.tsx").
     4. Try to make a DELETE request to the extracted URL with the [`data`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FdeleteAccountAPI.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FdeleteAccountAPI.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FdeleteAccountAPI.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A3%2C%22character%22%3A0%7D%5D "src/apis/deleteAccountAPI.tsx") object.
     5. If the request is successful, log and return the response data.
     6. If an error occurs during the DELETE request, log the error and return an empty array.

### Code
```tsx
import axios from "axios";
import { API_BASE_URL } from "../utils/constants";

let data = {
  UniqueId: "*",
};

/**
 * Fetches configuration data from a `configuration.json` file.
 * @returns {Promise<any>} A promise that resolves to the configuration data in JSON format if successful, otherwise `null`.
 */
export const getConfiguration = async () => {
  try {
    const response = await fetch(`configuration.json`, {
      method: "GET",
    });
    if (response.ok) {
      return response.json();
    }
  } catch (error) {
    console.log(error);
  }
  return null;
};

/**
 * Deletes an account by making a DELETE request to a specified URL.
 * @param {any} accountIdVar - The unique ID of the account to be deleted.
 * @returns {Promise<any>} A promise that resolves to the response data if successful, otherwise an empty array.
 */
export const deleteAccount = async (accountIdVar: any): Promise<any> => {
  const configuration = await getConfiguration();
  const url = configuration.api.cloudpulse_delete_account;
  // const url =
  //   "https://v178drbvvf.execute-api.us-east-1.amazonaws.com/production/deleteaccount";
  data.UniqueId = accountIdVar;
  try {
    const response = await axios.delete(url, { data });
    console.log(response);
    return response?.data;
  } catch (error) {
    console.log(error);
    return [];
  }
};
```

sumitchohan: generate detailed documentation for current code file including all methods, fields, properties and summary of their implementation including pseudo logic

GitHub Copilot: ### File: [`deleteRegionAPI.tsx`](command:_github.copilot.openRelativePath?%5B%7B%22scheme%22%3A%22file%22%2C%22authority%22%3A%22%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FdeleteRegionAPI.tsx%22%2C%22query%22%3A%22%22%2C%22fragment%22%3A%22%22%7D%5D "/Users/sumit/Downloads/Frontend/NAGISVPR/src/apis/deleteRegionAPI.tsx")

#### Summary
This file contains two main functions: [`getConfiguration`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FdeleteRegionAPI.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FdeleteRegionAPI.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FdeleteRegionAPI.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A8%2C%22character%22%3A0%7D%5D "src/apis/deleteRegionAPI.tsx") and [`deleteRegionAPI`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FdeleteRegionAPI.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FdeleteRegionAPI.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FdeleteRegionAPI.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A22%2C%22character%22%3A0%7D%5D "src/apis/deleteRegionAPI.tsx"). The purpose of these functions is to fetch configuration data and to delete a region using an API call, respectively. The file uses [`axios`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FdeleteRegionAPI.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FdeleteRegionAPI.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FdeleteRegionAPI.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A0%2C%22character%22%3A0%7D%5D "src/apis/deleteRegionAPI.tsx") for making HTTP requests and imports a base URL from a constants file.

#### Fields
- [`data`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FdeleteRegionAPI.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FdeleteRegionAPI.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FdeleteRegionAPI.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A3%2C%22character%22%3A0%7D%5D "src/apis/deleteRegionAPI.tsx"): An object with default values for [`AccountId`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FdeleteRegionAPI.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FdeleteRegionAPI.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FdeleteRegionAPI.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A4%2C%22character%22%3A2%7D%5D "src/apis/deleteRegionAPI.tsx") and [`Region`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FdeleteRegionAPI.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FdeleteRegionAPI.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FdeleteRegionAPI.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A5%2C%22character%22%3A2%7D%5D "src/apis/deleteRegionAPI.tsx"). This object is used to store the account ID and region name that will be sent in the API request.

#### Methods

1. **getConfiguration**
   - **Description**: Fetches configuration data from a local [`configuration.json`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FdeleteRegionAPI.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FdeleteRegionAPI.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FdeleteRegionAPI.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A26%2C%22character%22%3A2%7D%5D "src/apis/deleteRegionAPI.tsx") file.
   - **Parameters**: None
   - **Returns**: A promise that resolves to the configuration data if the fetch is successful, or `null` if an error occurs.
   - **Pseudo Logic**:
     1. Try to fetch [`configuration.json`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FdeleteRegionAPI.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FdeleteRegionAPI.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FdeleteRegionAPI.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A26%2C%22character%22%3A2%7D%5D "src/apis/deleteRegionAPI.tsx") using a GET request.
     2. If the response is OK, return the JSON data from the response.
     3. If an error occurs, log the error and return `null`.

2. **deleteRegionAPI**
   - **Description**: Deletes a region by making a POST request to a specified API endpoint.
   - **Parameters**:
     - [`account_id`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FdeleteRegionAPI.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FdeleteRegionAPI.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FdeleteRegionAPI.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A23%2C%22character%22%3A2%7D%5D "src/apis/deleteRegionAPI.tsx"): The ID of the account.
     - [`region_name`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FdeleteRegionAPI.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FdeleteRegionAPI.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FdeleteRegionAPI.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A24%2C%22character%22%3A2%7D%5D "src/apis/deleteRegionAPI.tsx"): The name of the region to be deleted.
   - **Returns**: A promise that resolves to the response data from the API call, or logs an error if the request fails.
   - **Pseudo Logic**:
     1. Call [`getConfiguration`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FdeleteRegionAPI.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FdeleteRegionAPI.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FdeleteRegionAPI.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A8%2C%22character%22%3A0%7D%5D "src/apis/deleteRegionAPI.tsx") to fetch the configuration data.
     2. Extract the `cloudpulse_delete_region` URL from the configuration data.
     3. Update the [`data`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FdeleteRegionAPI.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FdeleteRegionAPI.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FdeleteRegionAPI.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A3%2C%22character%22%3A0%7D%5D "src/apis/deleteRegionAPI.tsx") object with the provided [`account_id`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FdeleteRegionAPI.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FdeleteRegionAPI.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FdeleteRegionAPI.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A23%2C%22character%22%3A2%7D%5D "src/apis/deleteRegionAPI.tsx") and [`region_name`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FdeleteRegionAPI.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FdeleteRegionAPI.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FdeleteRegionAPI.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A24%2C%22character%22%3A2%7D%5D "src/apis/deleteRegionAPI.tsx").
     4. Log the [`data`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FdeleteRegionAPI.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FdeleteRegionAPI.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FdeleteRegionAPI.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A3%2C%22character%22%3A0%7D%5D "src/apis/deleteRegionAPI.tsx") object for debugging purposes.
     5. Try to make a POST request to the extracted URL with the [`data`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FdeleteRegionAPI.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FdeleteRegionAPI.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FdeleteRegionAPI.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A3%2C%22character%22%3A0%7D%5D "src/apis/deleteRegionAPI.tsx") object.
     6. Log the response and return the response data.
     7. If an error occurs, log the error.

### Code
```tsx
import axios from "axios";
import { API_BASE_URL } from "../utils/constants";

let data = {
  AccountId: "*",
  Region: "*",
};

/**
 * Fetches configuration data from a local configuration.json file.
 * @returns {Promise<any>} A promise that resolves to the configuration data or null if an error occurs.
 */
export const getConfiguration = async () => {
  try {
    const response = await fetch(`configuration.json`, {
      method: "GET",
    });
    if (response.ok) {
      return response.json();
    }
  } catch (error) {
    console.log(error);
  }
  return null;
};

/**
 * Deletes a region by making a POST request to a specified API endpoint.
 * @param {string} account_id - The ID of the account.
 * @param {string} region_name - The name of the region to be deleted.
 * @returns {Promise<any>} A promise that resolves to the response data from the API call.
 */
export const deleteRegionAPI = async (
  account_id,
  region_name
): Promise<any> => {
  const configuration = await getConfiguration();
  const url = configuration.api.cloudpulse_delete_region;

  data.AccountId = account_id;
  data.Region = region_name;

  console.log(data);
  try {
    const response = await axios.post(url, data);
    console.log(response);
    return response?.data;
  } catch (error) {
    console.log(error);
  }
};
```

sumitchohan: generate detailed documentation for current code file including all methods, fields, properties and summary of their implementation including pseudo logic

GitHub Copilot: ### File: getCommentRI.tsx

#### Summary
This file contains a function [`getCommentRI`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FgetCommentRI.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FgetCommentRI.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FgetCommentRI.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A3%2C%22character%22%3A0%7D%5D "src/apis/getCommentRI.tsx") which is responsible for fetching comments from a specified API endpoint. It uses the [`axios`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FgetCommentRI.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FgetCommentRI.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FgetCommentRI.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A0%2C%22character%22%3A0%7D%5D "src/apis/getCommentRI.tsx") library to make HTTP GET requests and retrieves configuration settings from another module.

#### Functions

1. **getCommentRI**
   - **Description**: This asynchronous function fetches comments related to a specific virtual machine (VM) and subscription from a remote API.
   - **Parameters**:
     - [`vmId`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FgetCommentRI.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FgetCommentRI.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FgetCommentRI.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A3%2C%22character%22%3A35%7D%5D "src/apis/getCommentRI.tsx") (string): The ID of the virtual machine.
     - [`subscription_id`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FgetCommentRI.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FgetCommentRI.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FgetCommentRI.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A3%2C%22character%22%3A49%7D%5D "src/apis/getCommentRI.tsx") (string): The ID of the subscription.
   - **Returns**: A promise that resolves to the data fetched from the API, or an empty array if an error occurs.
   - **Implementation**:
     - **Step 1**: Retrieve configuration settings by calling [`getConfiguration()`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FclientAPI.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A10%2C%22character%22%3A0%7D%5D "src/apis/clientAPI.tsx").
     - **Step 2**: Define the API endpoint URL.
     - **Step 3**: Make an HTTP GET request to the API endpoint using [`axios.get()`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FgetCommentRI.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FgetCommentRI.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FgetCommentRI.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A0%2C%22character%22%3A0%7D%5D "src/apis/getCommentRI.tsx"), passing [`vmId`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FgetCommentRI.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FgetCommentRI.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FgetCommentRI.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A3%2C%22character%22%3A35%7D%5D "src/apis/getCommentRI.tsx") and [`subscription_id`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FgetCommentRI.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FgetCommentRI.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FgetCommentRI.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A3%2C%22character%22%3A49%7D%5D "src/apis/getCommentRI.tsx") as query parameters.
     - **Step 4**: Log the API response data to the console.
     - **Step 5**: Return the response data.
     - **Error Handling**: If an error occurs during the API request, log the error to the console and return an empty array.

#### Pseudo Logic

```plaintext
Function getCommentRI(vmId, subscription_id):
    1. Call getConfiguration() to retrieve configuration settings.
    2. Define the API endpoint URL.
    3. Try to:
        a. Make an HTTP GET request to the API endpoint with vmId and subscription_id as query parameters.
        b. Log the API response data.
        c. Return the response data.
    4. Catch any errors:
        a. Log the error message.
        b. Return an empty array.
```

#### Code

```typescript
import axios from "axios";
import { getConfiguration } from "./clientAPI";

/**
 * Fetches comments related to a specific VM and subscription from a remote API.
 * 
 * @param {string} vmId - The ID of the virtual machine.
 * @param {string} subscription_id - The ID of the subscription.
 * @returns {Promise<any>} - A promise that resolves to the data fetched from the API, or an empty array if an error occurs.
 */
export const getCommentRI = async (vmId: string, subscription_id: string): Promise<any> => {
  const configuration = await getConfiguration();
  const url = "https://n1ewphxcxc.execute-api.us-east-1.amazonaws.com/dev/fetch-comment";

  try {
    const response = await axios.get(url, {
      params: {
        vm_id: vmId,
        subscription_id: subscription_id
      },
    });
    console.log("API Response:", response.data);
    return response.data;
  } catch (error) {
    console.error("Error fetching comments:", error);
    return [];
  }
};
```

sumitchohan: generate detailed documentation for current code file including all methods, fields, properties and summary of their implementation including pseudo logic

GitHub Copilot: ### File: getCommitedUsage.tsx

#### Summary
This file contains a function [`getCommittedUsage`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FgetCommitedUsage.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FgetCommitedUsage.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FgetCommitedUsage.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A3%2C%22character%22%3A0%7D%5D "src/apis/getCommitedUsage.tsx") which is responsible for fetching committed usage data from a specified API endpoint. It uses the [`axios`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FgetCommitedUsage.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FgetCommitedUsage.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FgetCommitedUsage.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A0%2C%22character%22%3A0%7D%5D "src/apis/getCommitedUsage.tsx") library to make HTTP GET requests and retrieves configuration settings from a local module [`clientAPI`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FclientAPI.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A0%2C%22character%22%3A0%7D%5D "src/apis/clientAPI.tsx").

#### Functions

1. **getCommittedUsage**

   **Description**: 
   This asynchronous function fetches committed usage data for a given tenant ID from a remote API endpoint.

   **Parameters**:
   - [`tenant_Id`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FgetCommitedUsage.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FgetCommitedUsage.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FgetCommitedUsage.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A3%2C%22character%22%3A40%7D%5D "src/apis/getCommitedUsage.tsx") (string): The ID of the tenant for which the committed usage data is to be fetched.

   **Returns**:
   - [`Promise<any>`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22external%22%3A%22file%3A%2F%2F%2FApplications%2FVisual%2520Studio%2520Code.app%2FContents%2FResources%2Fapp%2Fextensions%2Fnode_modules%2Ftypescript%2Flib%2Flib.es2015.iterable.d.ts%22%2C%22path%22%3A%22%2FApplications%2FVisual%20Studio%20Code.app%2FContents%2FResources%2Fapp%2Fextensions%2Fnode_modules%2Ftypescript%2Flib%2Flib.es2015.iterable.d.ts%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A215%2C%22character%22%3A0%7D%5D "../../../../../Applications/Visual Studio Code.app/Contents/Resources/app/extensions/node_modules/typescript/lib/lib.es2015.iterable.d.ts"): A promise that resolves to the committed usage data or an empty array in case of an error.

   **Pseudo Logic**:
   1. Import the [`axios`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FgetCommitedUsage.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FgetCommitedUsage.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FgetCommitedUsage.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A0%2C%22character%22%3A0%7D%5D "src/apis/getCommitedUsage.tsx") library for making HTTP requests.
   2. Import the [`getConfiguration`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FclientAPI.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A10%2C%22character%22%3A0%7D%5D "src/apis/clientAPI.tsx") function from the [`clientAPI`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FclientAPI.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A0%2C%22character%22%3A0%7D%5D "src/apis/clientAPI.tsx") module.
   3. Define the [`getCommittedUsage`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FgetCommitedUsage.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FgetCommitedUsage.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FgetCommitedUsage.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A3%2C%22character%22%3A0%7D%5D "src/apis/getCommitedUsage.tsx") function as an asynchronous function.
   4. Inside the function, call [`getConfiguration`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FclientAPI.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A10%2C%22character%22%3A0%7D%5D "src/apis/clientAPI.tsx") to retrieve configuration settings.
   5. Define the URL for the API endpoint.
   6. Use a `try-catch` block to handle the HTTP request:
      - In the `try` block:
        - Use [`axios.get`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FgetCommitedUsage.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FgetCommitedUsage.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FgetCommitedUsage.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A0%2C%22character%22%3A0%7D%5D "src/apis/getCommitedUsage.tsx") to make a GET request to the API endpoint with the tenant ID as a query parameter.
        - Return the `data` property from the response.
      - In the `catch` block:
        - Log any errors to the console.
        - Return an empty array in case of an error.

#### Code
```tsx
import axios from "axios";
import { getConfiguration } from "./clientAPI";

/**
 * Fetches committed usage data for a given tenant ID.
 *
 * @param {string} tenant_Id - The ID of the tenant for which the committed usage data is to be fetched.
 * @returns {Promise<any>} - A promise that resolves to the committed usage data or an empty array in case of an error.
 */
export const getCommittedUsage = async (tenant_Id: string): Promise<any> => {
  // Retrieve configuration settings
  const configuration = await getConfiguration();
  
  // Define the API endpoint URL
  const url = "https://k7au3c9rk3.execute-api.us-east-1.amazonaws.com/dev/get-committed-usage-data";
  
  try {
    // Make a GET request to the API endpoint with the tenant ID as a query parameter
    const response = await axios.get(url, {
      params: {
        tenantId: tenant_Id,
      },
    });
    
    // Return the data from the response
    return response.data.data;
  } catch (error) {
    // Log any errors to the console
    console.log(error);
    
    // Return an empty array in case of an error
    return [];
  }
};
```

This documentation provides a detailed overview of the `getCommittedUsage` function, including its purpose, parameters, return value, and the logic it follows to achieve its goal.

sumitchohan: generate detailed documentation for current code file including all methods, fields, properties and summary of their implementation including pseudo logic

GitHub Copilot: ### File: getRISPFilters.tsx

#### Summary
This file contains a function [`getFiltersRI`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FgetRISPFilters.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FgetRISPFilters.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FgetRISPFilters.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A3%2C%22character%22%3A0%7D%5D "src/apis/getRISPFilters.tsx") which fetches filter data from a remote API using Axios. The function is asynchronous and handles both successful and failed API requests.

#### Functions

1. **getFiltersRI**
   - **Description**: Fetches filter data for a given tenant ID from a remote API.
   - **Parameters**:
     - [`tenantId`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FgetRISPFilters.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FgetRISPFilters.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FgetRISPFilters.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A3%2C%22character%22%3A35%7D%5D "src/apis/getRISPFilters.tsx") (string): The ID of the tenant for which the filter data is being requested.
   - **Returns**: A promise that resolves to the filter data (type `any`).
   - **Pseudo Logic**:
     1. Call [`getConfiguration`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FclientAPI.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A10%2C%22character%22%3A0%7D%5D "src/apis/clientAPI.tsx") to retrieve configuration settings.
     2. Define the URL for the API endpoint.
     3. Use Axios to send a GET request to the API endpoint with [`tenantId`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FgetRISPFilters.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FgetRISPFilters.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FgetRISPFilters.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A3%2C%22character%22%3A35%7D%5D "src/apis/getRISPFilters.tsx") as a query parameter.
     4. If the request is successful, return the response data.
     5. If the request fails, catch the error and return an empty array.

#### Dependencies
- **axios**: A promise-based HTTP client for making requests.
- **getConfiguration**: A function imported from `./clientAPI` that retrieves configuration settings.

#### Code
```tsx
import axios from "axios";
import { getConfiguration } from "./clientAPI";

/**
 * Fetches filter data for a given tenant ID from a remote API.
 * 
 * @param {string} tenantId - The ID of the tenant for which the filter data is being requested.
 * @returns {Promise<any>} A promise that resolves to the filter data.
 */
export const getFiltersRI = async (tenantId: string): Promise<any> => {
  // Retrieve configuration settings
  const configuration = await getConfiguration();
  
  // Define the API endpoint URL
  const url = "https://aw9cl7kijh.execute-api.us-east-1.amazonaws.com/dev/filter-data";

  try {
    // Send a GET request to the API endpoint with tenantId as a query parameter
    const response = await axios.get(url, {
      params: {
        tenantId: tenantId,
      },
    });
    // Return the response data if the request is successful
    return response.data;
  } catch (error) {
    // Return an empty array if the request fails
    return [];
  }
};
```

This documentation provides a detailed overview of the `getFiltersRI` function, including its purpose, parameters, return value, and the logic it follows.

sumitchohan: generate detailed documentation for current code file including all methods, fields, properties and summary of their implementation including pseudo logic

GitHub Copilot: ### File: getTenentAPI.tsx

#### Summary
This file contains functions to fetch tenant data and committed usage data from a remote API using Axios. The functions are asynchronous and handle both successful and failed API requests.

#### Types

1. **Tenants**
   - **Description**: A type alias for a record where the keys and values are strings.

#### Functions

1. **getAllTenants**
   - **Description**: Fetches all tenants from a remote API.
   - **Parameters**: None
   - **Returns**: A promise that resolves to an object of type [`Tenants`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FgetTenentAPI.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FgetTenentAPI.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FgetTenentAPI.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A20%2C%22character%22%3A0%7D%5D "src/apis/getTenentAPI.tsx").
   - **Pseudo Logic**:
     1. Call [`getConfiguration`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FclientAPI.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A10%2C%22character%22%3A0%7D%5D "src/apis/clientAPI.tsx") to retrieve configuration settings.
     2. Define the URL for the API endpoint.
     3. Use Axios to send a GET request to the API endpoint.
     4. If the request is successful, return the `tenants` object from the response data.
     5. If the request fails, catch the error, log it, and return an empty object.

2. **getAllCommittedUsage**
   - **Description**: Fetches all committed usage data from a remote API.
   - **Parameters**: None
   - **Returns**: A promise that resolves to the committed usage data (type `any`).
   - **Pseudo Logic**:
     1. Call [`getConfiguration`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FclientAPI.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A10%2C%22character%22%3A0%7D%5D "src/apis/clientAPI.tsx") to retrieve configuration settings.
     2. Define the URL for the API endpoint.
     3. Use Axios to send a GET request to the API endpoint.
     4. If the request is successful, return the response data.
     5. If the request fails, catch the error, log it, and return an empty object.

#### Dependencies
- **axios**: A promise-based HTTP client for making requests.
- **getConfiguration**: A function imported from `./clientAPI` that retrieves configuration settings.

#### Code
```tsx
import axios from "axios";
import { getConfiguration } from "./clientAPI";

/**
 * A type alias for a record where the keys and values are strings.
 */
type Tenants = Record<string, string>;

/**
 * Fetches all tenants from a remote API.
 * 
 * @returns {Promise<Tenants>} A promise that resolves to an object of type Tenants.
 */
export const getAllTenants = async (): Promise<Tenants> => {
  // Retrieve configuration settings
  const configuration = await getConfiguration();
  
  // Define the API endpoint URL
  const url =
    "https://k7au3c9rk3.execute-api.us-east-1.amazonaws.com/dev/get-committed-usage-data";

  try {
    // Send a GET request to the API endpoint
    const response = await axios.get(url);
    
    // Return the tenants object if the request is successful
    return response.data.tenants;
  } catch (error) {
    // Log the error and return an empty object if the request fails
    console.log(error);
    return {};
  }
};

/**
 * Fetches all committed usage data from a remote API.
 * 
 * @returns {Promise<any>} A promise that resolves to the committed usage data.
 */
export const getAllCommittedUsage = async (): Promise<any> => {
  // Retrieve configuration settings
  const configuration = await getConfiguration();
  
  // Define the API endpoint URL
  const url =
    "https://k7au3c9rk3.execute-api.us-east-1.amazonaws.com/dev/get-committed-usage-data";

  try {
    // Send a GET request to the API endpoint
    const response = await axios.get(url);
    
    // Return the response data if the request is successful
    return response.data;
  } catch (error) {
    // Log the error and return an empty object if the request fails
    console.log(error);
    return {};
  }
};
```

This documentation provides a detailed overview of the `getAllTenants` and `getAllCommittedUsage` functions, including their purpose, parameters, return values, and the logic they follow.

sumitchohan: generate detailed documentation for current code file including all methods, fields, properties and summary of their implementation including pseudo logic


GitHub Copilot: ### File: governanceAPI.tsx

#### Summary
This file contains functions to fetch configuration data and governance data from a remote API. The functions are asynchronous and handle both successful and failed API requests.

#### Functions

1. **getConfiguration**
   - **Description**: Fetches configuration data from a local JSON file.
   - **Parameters**: None
   - **Returns**: A promise that resolves to the configuration data (type `any`) or `null` if the request fails.
   - **Pseudo Logic**:
     1. Send a GET request to fetch [`configuration.json`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FgovernanceAPI.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FgovernanceAPI.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FgovernanceAPI.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A21%2C%22character%22%3A2%7D%5D "src/apis/governanceAPI.tsx").
     2. If the response is successful, return the parsed JSON data.
     3. If the request fails, catch the error, log it, and return `null`.

2. **getGovernance**
   - **Description**: Fetches governance data from a remote API based on the provided account ID and cloud provider.
   - **Parameters**:
     - [`accountId`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FgovernanceAPI.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FgovernanceAPI.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FgovernanceAPI.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A18%2C%22character%22%3A2%7D%5D "src/apis/governanceAPI.tsx") (string): The account ID for which to fetch governance data.
     - [`cloudProvider`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FgovernanceAPI.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FgovernanceAPI.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FgovernanceAPI.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A19%2C%22character%22%3A2%7D%5D "src/apis/governanceAPI.tsx") (string): The cloud provider for which to fetch governance data.
   - **Returns**: A promise that resolves to the governance data (type `any`) or an empty array if the request fails.
   - **Pseudo Logic**:
     1. Call [`getConfiguration`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FgovernanceAPI.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FgovernanceAPI.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FgovernanceAPI.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A3%2C%22character%22%3A0%7D%5D "src/apis/governanceAPI.tsx") to retrieve configuration settings.
     2. Extract the `cloudpulse_governance` URL from the configuration.
     3. Define the request body with [`accountId`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FgovernanceAPI.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FgovernanceAPI.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FgovernanceAPI.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A18%2C%22character%22%3A2%7D%5D "src/apis/governanceAPI.tsx") and [`cloudProvider`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FgovernanceAPI.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FgovernanceAPI.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FgovernanceAPI.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A19%2C%22character%22%3A2%7D%5D "src/apis/governanceAPI.tsx").
     4. Use Axios to send a POST request to the API endpoint with the request body.
     5. If the request is successful, return the response data.
     6. If the request fails, catch the error, log it, and return an empty array.

#### Dependencies
- **axios**: A promise-based HTTP client for making requests.
- **API_BASE_URL**: A constant imported from `../utils/constants`.

#### Code
```tsx
import axios from "axios";
import { API_BASE_URL } from "../utils/constants";

/**
 * Fetches configuration data from a local JSON file.
 * 
 * @returns {Promise<any>} A promise that resolves to the configuration data or null if the request fails.
 */
export const getConfiguration = async () => {
  try {
    // Send a GET request to fetch configuration.json
    const response = await fetch(`configuration.json`, {
      method: "GET",
    });
    
    // If the response is successful, return the parsed JSON data
    if (response.ok) {
      return response.json();
    }
  } catch (error) {
    // Log the error and return null if the request fails
    console.log(error);
  }
  
  return null;
};

/**
 * Fetches governance data from a remote API based on the provided account ID and cloud provider.
 * 
 * @param {string} accountId - The account ID for which to fetch governance data.
 * @param {string} cloudProvider - The cloud provider for which to fetch governance data.
 * @returns {Promise<any>} A promise that resolves to the governance data or an empty array if the request fails.
 */
export const getGovernance = async (
  accountId: string,
  cloudProvider: string
): Promise<any> => {
  // Retrieve configuration settings
  const configuration = await getConfiguration();
  
  // Extract the cloudpulse_governance URL from the configuration
  const url = configuration.api.cloudpulse_governance;
  
  // Define the request body with accountId and cloudProvider
  const body = { AccountId: accountId, cloud_provider: cloudProvider };

  try {
    // Send a POST request to the API endpoint with the request body
    const response = await axios.post(url, body);
    
    // Return the response data if the request is successful
    return response?.data;
  } catch (error) {
    // Log the error and return an empty array if the request fails
    console.log(error);
    return [];
  }
};
```

This documentation provides a detailed overview of the `getConfiguration` and `getGovernance` functions, including their purpose, parameters, return values, and the logic they follow.

sumitchohan: generate detailed documentation for current code file including all methods, fields, properties and summary of their implementation including pseudo logic

GitHub Copilot: ### Documentation for [`highestCostSavings.tsx`](command:_github.copilot.openRelativePath?%5B%7B%22scheme%22%3A%22file%22%2C%22authority%22%3A%22%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FhighestCostSavings.tsx%22%2C%22query%22%3A%22%22%2C%22fragment%22%3A%22%22%7D%5D "/Users/sumit/Downloads/Frontend/NAGISVPR/src/apis/highestCostSavings.tsx")

#### Summary
This file contains two main functions: [`getConfiguration`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FhighestCostSavings.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FhighestCostSavings.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FhighestCostSavings.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A4%2C%22character%22%3A0%7D%5D "src/apis/highestCostSavings.tsx") and [`highestCostSavings`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FhighestCostSavings.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FhighestCostSavings.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FhighestCostSavings.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A18%2C%22character%22%3A0%7D%5D "src/apis/highestCostSavings.tsx"). These functions are used to fetch configuration data and to retrieve the highest cost savings data from a cloud provider, respectively.

#### Functions

1. **getConfiguration**
   - **Description**: Fetches configuration data from a local JSON file.
   - **Parameters**: None
   - **Returns**: A promise that resolves to the configuration data in JSON format, or `null` if an error occurs.
   - **Pseudo Logic**:
     1. Attempt to fetch the [`configuration.json`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FhighestCostSavings.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FhighestCostSavings.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FhighestCostSavings.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A22%2C%22character%22%3A2%7D%5D "src/apis/highestCostSavings.tsx") file using a GET request.
     2. If the response is successful ([`response.ok`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FhighestCostSavings.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FhighestCostSavings.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FhighestCostSavings.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A6%2C%22character%22%3A4%7D%5D "src/apis/highestCostSavings.tsx")), parse and return the JSON data.
     3. If an error occurs during the fetch operation, log the error to the console and return `null`.

2. **highestCostSavings**
   - **Description**: Retrieves the highest cost savings data for a given account and cloud provider.
   - **Parameters**:
     - [`accountId`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FhighestCostSavings.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FhighestCostSavings.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FhighestCostSavings.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A19%2C%22character%22%3A2%7D%5D "src/apis/highestCostSavings.tsx") (string): The ID of the account for which to retrieve cost savings data.
     - [`cloudProvider`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FhighestCostSavings.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FhighestCostSavings.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FhighestCostSavings.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A20%2C%22character%22%3A2%7D%5D "src/apis/highestCostSavings.tsx") (string): The cloud provider for which to retrieve cost savings data.
   - **Returns**: A promise that resolves to the highest cost savings data, or an empty array if an error occurs.
   - **Pseudo Logic**:
     1. Call [`getConfiguration`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FhighestCostSavings.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FhighestCostSavings.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FhighestCostSavings.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A4%2C%22character%22%3A0%7D%5D "src/apis/highestCostSavings.tsx") to retrieve the configuration data.
     2. Extract the URL for the highest cost savings API from the configuration data.
     3. Construct the request body with [`accountId`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FhighestCostSavings.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FhighestCostSavings.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FhighestCostSavings.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A19%2C%22character%22%3A2%7D%5D "src/apis/highestCostSavings.tsx") and [`cloudProvider`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FhighestCostSavings.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FhighestCostSavings.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FhighestCostSavings.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A20%2C%22character%22%3A2%7D%5D "src/apis/highestCostSavings.tsx").
     4. Attempt to send a POST request to the API endpoint with the constructed body.
     5. If the request is successful, return the data from the response.
     6. If an error occurs during the request, log the error to the console and return an empty array.

#### Code

```tsx
import axios from "axios";
import { API_BASE_URL } from "../utils/constants";
import { highestCostData } from "../utils/interfaces";

/**
 * Fetches configuration data from a local JSON file.
 * @returns {Promise<any>} A promise that resolves to the configuration data in JSON format, or null if an error occurs.
 */
export const getConfiguration = async () => {
  try {
    const response = await fetch(`configuration.json`, {
      method: "GET",
    });
    if (response.ok) {
      return response.json();
    }
  } catch (error) {
    console.log(error);
  }
  return null;
};

/**
 * Retrieves the highest cost savings data for a given account and cloud provider.
 * @param {string} accountId - The ID of the account for which to retrieve cost savings data.
 * @param {string} cloudProvider - The cloud provider for which to retrieve cost savings data.
 * @returns {Promise<any>} A promise that resolves to the highest cost savings data, or an empty array if an error occurs.
 */
export const highestCostSavings = async (
  accountId: string,
  cloudProvider: string
): Promise<any> => {
  const configuration = await getConfiguration();
  const url = configuration.api.cloudpulse_higest_costsavings;

  try {
    const body = {
      AccountId: accountId,
      cloud_provider: cloudProvider,
    };
    // console.log(body);
    var response = await axios.post(url, body);
    return response?.data.body;
  } catch (error) {
    console.log(error);
    return [];
  }
};
```

This documentation provides a detailed overview of the functions, their parameters, return values, and the logic implemented within each function.

sumitchohan: generate detailed documentation for current code file including all methods, fields, properties and summary of their implementation including pseudo logic

GitHub Copilot: ### Documentation for [`jiraTicketAPI.tsx`](command:_github.copilot.openRelativePath?%5B%7B%22scheme%22%3A%22file%22%2C%22authority%22%3A%22%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FjiraTicketAPI.tsx%22%2C%22query%22%3A%22%22%2C%22fragment%22%3A%22%22%7D%5D "/Users/sumit/Downloads/Frontend/NAGISVPR/src/apis/jiraTicketAPI.tsx")

#### Summary
This file contains functions to interact with a JIRA ticket creation API. It includes methods to fetch configuration data and create JIRA tickets using the provided configuration.

#### Fields
- [`data`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FjiraTicketAPI.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FjiraTicketAPI.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FjiraTicketAPI.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A3%2C%22character%22%3A0%7D%5D "src/apis/jiraTicketAPI.tsx"): An object that holds the necessary information for creating a JIRA ticket.
  ```typescript
  let data = {
    policy_id: "",
    account: "",
    comment: "",
    account_id: "",
  };
  ```

#### Methods

1. **getConfiguration**
   - **Description**: Fetches configuration data from a `configuration.json` file.
   - **Parameters**: None
   - **Returns**: A promise that resolves to the configuration data if the fetch is successful, otherwise `null`.
   - **Pseudo Logic**:
     1. Attempt to fetch `configuration.json` with a GET request.
     2. If the response is successful, return the JSON data.
     3. If an error occurs, log the error and return `null`.

   ```typescript
   export const getConfiguration = async () => {
     try {
       const response = await fetch(`configuration.json`, {
         method: "GET",
       });
       if (response.ok) {
         return response.json();
       }
     } catch (error) {
       console.log(error);
     }
     return null;
   };
   ```

2. **creatJiraTicket**
   - **Description**: Creates a JIRA ticket using the provided properties and configuration data.
   - **Parameters**:
     - `props`: An object containing the policy information.
     - `accountVar`: The account variable.
     - `commentVar`: The comment variable.
     - `accountIdVar`: The account ID variable.
   - **Returns**: A promise that resolves to the result of the JIRA ticket creation API call.
   - **Pseudo Logic**:
     1. Fetch the configuration data using `getConfiguration`.
     2. Extract the JIRA ticket creation URL from the configuration.
     3. Populate the `data` object with the provided parameters.
     4. Log the `data` object for debugging purposes.
     5. Attempt to create a JIRA ticket using the populated `data` and the extracted URL.
     6. Handle any errors that occur during the API call.

   ```typescript
   export const creatJiraTicket = async (
     props,
     accountVar,
     commentVar,
     accountIdVar
   ): Promise<any> => {
     const configuration = await getConfiguration();
     const url = configuration.api.cloudpulse_create_jira_ticket;
     var policyId = props.policy.policy_id;
     data.policy_id = policyId;
     data.account = accountVar;
     data.comment = commentVar;
     data.account_id = accountIdVar;
     console.log(data);
     try {
       // API call logic here
     } catch (error) {
       // Error handling logic here
     }
   };
   ```

#### Notes
- The `creatJiraTicket` function currently has a commented-out URL and incomplete API call logic.
- Error handling is minimal and could be improved for better robustness.
- The `data` object is mutable and shared across function calls, which might lead to potential issues in a concurrent environment.

sumitchohan: generate detailed documentation for current code file including all methods, fields, properties and summary of their implementation including pseudo logic

GitHub Copilot: ### Documentation for [`loginAPI.tsx`](command:_github.copilot.openRelativePath?%5B%7B%22scheme%22%3A%22file%22%2C%22authority%22%3A%22%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FloginAPI.tsx%22%2C%22query%22%3A%22%22%2C%22fragment%22%3A%22%22%7D%5D "/Users/sumit/Downloads/Frontend/NAGISVPR/src/apis/loginAPI.tsx")

#### Summary
This file contains functions to handle user login via Cognito and SSO (Single Sign-On). It includes methods to fetch configuration data, login using Cognito credentials, and login using an SSO code.

#### Methods

1. **getConfiguration**
   - **Description**: Fetches configuration data from a [`configuration.json`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FloginAPI.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FloginAPI.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FloginAPI.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A22%2C%22character%22%3A4%7D%5D "src/apis/loginAPI.tsx") file.
   - **Parameters**: None
   - **Returns**: A promise that resolves to the configuration data if the fetch is successful, otherwise `null`.
   - **Pseudo Logic**:
     1. Attempt to fetch [`configuration.json`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FloginAPI.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FloginAPI.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FloginAPI.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A22%2C%22character%22%3A4%7D%5D "src/apis/loginAPI.tsx") with a GET request.
     2. If the response is successful, return the JSON data.
     3. If an error occurs, log the error and return `null`.

   ```typescript
   export const getConfiguration = async () => {
     try {
       const response = await fetch(`configuration.json`, {
         method: "GET",
       });
       if (response.ok) {
         return response.json();
       }
     } catch (error) {
       console.log(error);
     }
     return null;
   };
   ```

2. **loginCognito**
   - **Description**: Logs in a user using Cognito credentials.
   - **Parameters**:
     - `username`: The username of the user.
     - `password`: The password of the user.
   - **Returns**: A promise that resolves to an `ApiResponseLogin` object if the login is successful, otherwise `null`.
   - **Pseudo Logic**:
     1. Fetch the configuration data using `getConfiguration`.
     2. Extract the Cognito login URL from the configuration.
     3. Create a request body with the username and password.
     4. Send a POST request to the Cognito login URL with the request body.
     5. If the response is successful, return the response data.
     6. If an error occurs, log the error and return `null`.

   ```typescript
   export const loginCognito = async (
     username: string,
     password: string
   ): Promise<ApiResponseLogin | null> => {
     try {
       const configuration = await getConfiguration();
       const url = configuration.api.cloudpulse_login_cognito;
       const body = { username: username, password: password };
       const response = await axios.post(url, body);
       return response.data;
     } catch (error) {
       console.log(error);
       return null;
     }
   };
   ```

3. **loginSSO**
   - **Description**: Logs in a user using an SSO code.
   - **Parameters**:
     - [`code`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FloginAPI.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FloginAPI.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FloginAPI.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A34%2C%22character%22%3A2%7D%5D "src/apis/loginAPI.tsx"): The SSO code.
   - **Returns**: A promise that resolves to an [`ApiResponseLogin`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Futils%2Finterfaces.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A58%2C%22character%22%3A0%7D%5D "src/utils/interfaces.tsx") object if the login is successful, otherwise `null`.
   - **Pseudo Logic**:
     1. Fetch the configuration data using [`getConfiguration`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FloginAPI.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FloginAPI.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FloginAPI.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A3%2C%22character%22%3A0%7D%5D "src/apis/loginAPI.tsx").
     2. Extract the SSO login URL from the configuration.
     3. Create a request body with the SSO code.
     4. Send a POST request to the SSO login URL with the request body.
     5. If the response is successful, return the response data.
     6. If an error occurs, log the error and return `null`.

   ```typescript
   export const loginSSO = async (
     code: string
   ): Promise<ApiResponseLogin | null> => {
     try {
       const configuration = await getConfiguration();
       const url = configuration.api.cloudpulse_login_sso;
       const body = { code: code };
       const response = await axios.post(url, body);
       return response.data;
     } catch (error) {
       console.log(error);
       return null;
     }
   };
   ```

#### Notes
- The `getConfiguration` function is used by both `loginCognito` and `loginSSO` to fetch the necessary configuration data.
- Error handling is minimal and could be improved for better robustness.
- The `axios` library is used for making HTTP POST requests.

sumitchohan: generate detailed documentation for current code file including all methods, fields, properties and summary of their implementation including pseudo logic

GitHub Copilot: ### Documentation for [`MarkExceptionAPI.tsx`](command:_github.copilot.openRelativePath?%5B%7B%22scheme%22%3A%22file%22%2C%22authority%22%3A%22%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FMarkExceptionAPI.tsx%22%2C%22query%22%3A%22%22%2C%22fragment%22%3A%22%22%7D%5D "/Users/sumit/Downloads/Frontend/NAGISVPR/src/apis/MarkExceptionAPI.tsx")

#### Summary
This file contains functions to handle marking exceptions via an API. It includes methods to fetch configuration data and to mark an exception using provided parameters.

#### Fields

1. **data**
   - **Type**: Object
   - **Description**: A template object containing default values for the API request payload.
   - **Properties**:
     - [`region`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FMarkExceptionAPI.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FMarkExceptionAPI.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FMarkExceptionAPI.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A3%2C%22character%22%3A2%7D%5D "src/apis/MarkExceptionAPI.tsx"): Default value is `"*"`.
     - [`accountid`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FMarkExceptionAPI.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FMarkExceptionAPI.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FMarkExceptionAPI.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A4%2C%22character%22%3A2%7D%5D "src/apis/MarkExceptionAPI.tsx"): Default value is `"*"`.
     - [`policyid`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FMarkExceptionAPI.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FMarkExceptionAPI.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FMarkExceptionAPI.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A5%2C%22character%22%3A2%7D%5D "src/apis/MarkExceptionAPI.tsx"): Default value is `"*"`.
     - [`uniqueid`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FMarkExceptionAPI.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FMarkExceptionAPI.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FMarkExceptionAPI.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A6%2C%22character%22%3A2%7D%5D "src/apis/MarkExceptionAPI.tsx"): An array containing objects with [`unique_id`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FMarkExceptionAPI.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FMarkExceptionAPI.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FMarkExceptionAPI.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A8%2C%22character%22%3A6%7D%5D "src/apis/MarkExceptionAPI.tsx") and [`Region`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FMarkExceptionAPI.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FMarkExceptionAPI.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FMarkExceptionAPI.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A9%2C%22character%22%3A6%7D%5D "src/apis/MarkExceptionAPI.tsx"), both defaulting to `"*"`.
     - [`email`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FMarkExceptionAPI.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FMarkExceptionAPI.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FMarkExceptionAPI.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A12%2C%22character%22%3A2%7D%5D "src/apis/MarkExceptionAPI.tsx"): Default value is `"*"`.

   ```typescript
   let data = {
     region: "*",
     accountid: "*",
     policyid: "*",
     uniqueid: [
       {
         unique_id: "*",
         Region: "*",
       },
     ],
     email: "*",
   };
   ```

#### Methods

1. **getConfiguration**
   - **Description**: Fetches configuration data from a `configuration.json` file.
   - **Parameters**: None
   - **Returns**: A promise that resolves to the configuration data if the fetch is successful, otherwise `null`.
   - **Pseudo Logic**:
     1. Attempt to fetch `configuration.json` with a GET request.
     2. If the response is successful, return the JSON data.
     3. If an error occurs, log the error and return `null`.

   ```typescript
   export const getConfiguration = async () => {
     try {
       const response = await fetch([`configuration.json`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FMarkExceptionAPI.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FMarkExceptionAPI.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FMarkExceptionAPI.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A36%2C%22character%22%3A2%7D%5D "src/apis/MarkExceptionAPI.tsx"), {
         method: "GET",
       });
       if (response.ok) {
         return response.json();
       }
     } catch (error) {
       console.log(error);
     }
     return null;
   };
   ```

2. **markExceptionAPI**
   - **Description**: Marks an exception using the provided parameters and sends a request to the API.
   - **Parameters**:
     - [`accountId`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FMarkExceptionAPI.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FMarkExceptionAPI.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FMarkExceptionAPI.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A30%2C%22character%22%3A2%7D%5D "src/apis/MarkExceptionAPI.tsx"): The account ID to be used in the request.
     - [`policyId`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FMarkExceptionAPI.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FMarkExceptionAPI.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FMarkExceptionAPI.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A31%2C%22character%22%3A2%7D%5D "src/apis/MarkExceptionAPI.tsx"): The policy ID to be used in the request.
     - [`uniqueID`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FMarkExceptionAPI.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FMarkExceptionAPI.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FMarkExceptionAPI.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A32%2C%22character%22%3A2%7D%5D "src/apis/MarkExceptionAPI.tsx"): The unique ID to be used in the request.
     - [`emailVar`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FMarkExceptionAPI.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FMarkExceptionAPI.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FMarkExceptionAPI.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A33%2C%22character%22%3A2%7D%5D "src/apis/MarkExceptionAPI.tsx"): The email to be used in the request.
     - [`reportRegion`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FMarkExceptionAPI.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FMarkExceptionAPI.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FMarkExceptionAPI.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A34%2C%22character%22%3A2%7D%5D "src/apis/MarkExceptionAPI.tsx"): The region to be used in the request.
   - **Returns**: A promise that resolves to the API response if the request is successful, otherwise `null`.
   - **Pseudo Logic**:
     1. Fetch the configuration data using [`getConfiguration`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FMarkExceptionAPI.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FMarkExceptionAPI.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FMarkExceptionAPI.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A15%2C%22character%22%3A0%7D%5D "src/apis/MarkExceptionAPI.tsx").
     2. Set the API endpoint URL (currently hardcoded).
     3. Update the [`data`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FMarkExceptionAPI.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FMarkExceptionAPI.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FMarkExceptionAPI.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A2%2C%22character%22%3A0%7D%5D "src/apis/MarkExceptionAPI.tsx") object with the provided parameters.
     4. Send a POST request to the API endpoint with the updated [`data`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FMarkExceptionAPI.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FMarkExceptionAPI.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FMarkExceptionAPI.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A2%2C%22character%22%3A0%7D%5D "src/apis/MarkExceptionAPI.tsx") object.
     5. If the response is successful, return the response data.
     6. If an error occurs, log the error and return `null`.

   ```typescript
   export const markExceptionAPI = async (
     accountId,
     policyId,
     uniqueID,
     emailVar,
     reportRegion
   ): Promise<any> => {
     const configuration = await getConfiguration();
     //   const url = configuration.api.cloudpulse_delete_region;
     const url =
       "https://yptggcxn13.execute-api.us-east-1.amazonaws.com/dev/create-exception";
     data.accountid = accountId;
     data.policyid = policyId;
     data.uniqueid[0].unique_id = uniqueID;
     data.uniqueid[0].Region = reportRegion;
     data.email = emailVar;
     try {
       const response = await axios.post(url, data);
       return response.data;
     } catch (error) {
       console.log(error);
       return null;
     }
   };
   ```

#### Notes
- The `getConfiguration` function is used to fetch configuration data, although the URL in `markExceptionAPI` is currently hardcoded.
- Error handling is minimal and could be improved for better robustness.
- The `axios` library is used for making HTTP POST requests.

sumitchohan: generate detailed documentation for current code file including all methods, fields, properties and summary of their implementation including pseudo logic

GitHub Copilot: ### MarkIgnoreAPI.tsx Documentation

#### Summary
This file contains functions to interact with an API for marking items to be ignored until a specified date. It includes methods for fetching configuration data and marking items to be ignored using an API endpoint.

#### Fields and Properties

- **_days**: A variable of type `number` (currently unused).
- **data**: An object containing default values for region, account ID, policy ID, unique ID, and an ignored till date.

#### Methods

1. **getConfiguration**
   - **Description**: Fetches configuration data from a [`configuration.json`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FMarkIgnoreAPI.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FMarkIgnoreAPI.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FMarkIgnoreAPI.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A37%2C%22character%22%3A2%7D%5D "src/apis/MarkIgnoreAPI.tsx") file.
   - **Parameters**: None
   - **Returns**: A promise that resolves to the configuration data in JSON format, or `null` if an error occurs.
   - **Pseudo Logic**:
     1. Attempt to fetch the [`configuration.json`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FMarkIgnoreAPI.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FMarkIgnoreAPI.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FMarkIgnoreAPI.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A37%2C%22character%22%3A2%7D%5D "src/apis/MarkIgnoreAPI.tsx") file using a GET request.
     2. If the response is successful, return the JSON data.
     3. If an error occurs, log the error and return `null`.

2. **markIgnoreAPI**
   - **Description**: Marks an item to be ignored until a specified date by making a POST request to an API endpoint.
   - **Parameters**:
     - [`accountId`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FMarkIgnoreAPI.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FMarkIgnoreAPI.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FMarkIgnoreAPI.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A31%2C%22character%22%3A2%7D%5D "src/apis/MarkIgnoreAPI.tsx"): The account ID to be marked.
     - [`policyId`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FMarkIgnoreAPI.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FMarkIgnoreAPI.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FMarkIgnoreAPI.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A32%2C%22character%22%3A2%7D%5D "src/apis/MarkIgnoreAPI.tsx"): The policy ID to be marked.
     - [`uniqueID`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FMarkIgnoreAPI.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FMarkIgnoreAPI.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FMarkIgnoreAPI.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A33%2C%22character%22%3A2%7D%5D "src/apis/MarkIgnoreAPI.tsx"): The unique ID of the item to be marked.
     - [`Tilldate`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FMarkIgnoreAPI.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FMarkIgnoreAPI.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FMarkIgnoreAPI.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A34%2C%22character%22%3A2%7D%5D "src/apis/MarkIgnoreAPI.tsx"): The date until which the item should be ignored.
     - [`reportRegion`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FMarkIgnoreAPI.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FMarkIgnoreAPI.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FMarkIgnoreAPI.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A35%2C%22character%22%3A2%7D%5D "src/apis/MarkIgnoreAPI.tsx"): The region associated with the report.
   - **Returns**: A promise that resolves to the response from the API call.
   - **Pseudo Logic**:
     1. Fetch the configuration data using the [`getConfiguration`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FMarkIgnoreAPI.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FMarkIgnoreAPI.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FMarkIgnoreAPI.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A16%2C%22character%22%3A0%7D%5D "src/apis/MarkIgnoreAPI.tsx") method.
     2. Define the API endpoint URL (currently hardcoded).
     3. Make a POST request to the API endpoint with the necessary data to mark the item as ignored.

### Code

```tsx
import axios from "axios";

var _days: number;
let data = {
  region: "*",
  accountid: "*",
  policyid: "*",
  uniqueid: [
    {
      unique_id: "*",
      Region: "*",
    },
  ],
  "ignoredtill": "2024-09-15",
};

/**
 * Fetches configuration data from a configuration.json file.
 * @returns {Promise<any>} A promise that resolves to the configuration data in JSON format, or null if an error occurs.
 */
export const getConfiguration = async () => {
  try {
    const response = await fetch(`configuration.json`, {
      method: "GET",
    });
    if (response.ok) {
      return response.json();
    }
  } catch (error) {
    console.log(error);
  }
  return null;
};

/**
 * Marks an item to be ignored until a specified date by making a POST request to an API endpoint.
 * @param {string} accountId - The account ID to be marked.
 * @param {string} policyId - The policy ID to be marked.
 * @param {string} uniqueID - The unique ID of the item to be marked.
 * @param {string} Tilldate - The date until which the item should be ignored.
 * @param {string} reportRegion - The region associated with the report.
 * @returns {Promise<any>} A promise that resolves to the response from the API call.
 */
export const markIgnoreAPI = async (
  accountId,
  policyId,
  uniqueID,
  Tilldate,
  reportRegion
): Promise<any> => {
  const configuration = await getConfiguration();
  //   const url = configuration.api.cloudpulse_delete_region;
  const url =
    "https://nsn8vk3q3g.execute-api.us-east-1.amazonaws.com/dev/ignore";
};
```

sumitchohan: generate detailed documentation for current code file including all methods, fields, properties and summary of their implementation including pseudo logic

GitHub Copilot: ### File: optimiser.tsx

#### Summary
This file contains functions related to fetching and processing report data and configuration settings. The main functions include [`getReport`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2Foptimiser.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2Foptimiser.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2Foptimiser.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A2%2C%22character%22%3A0%7D%5D "src/apis/optimiser.tsx"), [`getConfiguration`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2Foptimiser.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2Foptimiser.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2Foptimiser.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A18%2C%22character%22%3A0%7D%5D "src/apis/optimiser.tsx"), and [`demoOptimiserData`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2Foptimiser.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2Foptimiser.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2Foptimiser.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A32%2C%22character%22%3A33%7D%5D "src/apis/optimiser.tsx").

#### Functions

1. **getReport**
   - **Description**: Asynchronously fetches a report based on the provided [`policyId`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2Foptimiser.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2Foptimiser.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2Foptimiser.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A3%2C%22character%22%3A2%7D%5D "src/apis/optimiser.tsx") and [`accountName`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2Foptimiser.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2Foptimiser.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2Foptimiser.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A4%2C%22character%22%3A2%7D%5D "src/apis/optimiser.tsx").
   - **Parameters**:
     - [`policyId`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2Foptimiser.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2Foptimiser.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2Foptimiser.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A3%2C%22character%22%3A2%7D%5D "src/apis/optimiser.tsx") (string): The ID of the policy for which the report is to be fetched.
     - [`accountName`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2Foptimiser.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2Foptimiser.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2Foptimiser.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A4%2C%22character%22%3A2%7D%5D "src/apis/optimiser.tsx") (string): The name of the account associated with the policy.
   - **Returns**: A [`Promise`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22external%22%3A%22file%3A%2F%2F%2FApplications%2FVisual%2520Studio%2520Code.app%2FContents%2FResources%2Fapp%2Fextensions%2Fnode_modules%2Ftypescript%2Flib%2Flib.es2015.iterable.d.ts%22%2C%22path%22%3A%22%2FApplications%2FVisual%20Studio%20Code.app%2FContents%2FResources%2Fapp%2Fextensions%2Fnode_modules%2Ftypescript%2Flib%2Flib.es2015.iterable.d.ts%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A215%2C%22character%22%3A0%7D%5D "../../../../../Applications/Visual Studio Code.app/Contents/Resources/app/extensions/node_modules/typescript/lib/lib.es2015.iterable.d.ts") that resolves to a [`ReportResponseFormat`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Futils%2Finterfaces.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A46%2C%22character%22%3A0%7D%5D "src/utils/interfaces.tsx") object.
   - **Pseudo Logic**:
     1. Call the [`demoOptimiserData`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2Foptimiser.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2Foptimiser.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2Foptimiser.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A32%2C%22character%22%3A33%7D%5D "src/apis/optimiser.tsx") function to get mock data.
     2. Return the mock data.
     3. (Commented out) Attempt to post data to a URL using `axios`.
     4. (Commented out) Handle the response and return the data.
     5. (Commented out) Catch any errors, log them, and return a default [`ReportResponseFormat`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Futils%2Finterfaces.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A46%2C%22character%22%3A0%7D%5D "src/utils/interfaces.tsx") object.

2. **getConfiguration**
   - **Description**: Asynchronously fetches configuration settings from a `configuration.json` file.
   - **Parameters**: None.
   - **Returns**: A [`Promise`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22external%22%3A%22file%3A%2F%2F%2FApplications%2FVisual%2520Studio%2520Code.app%2FContents%2FResources%2Fapp%2Fextensions%2Fnode_modules%2Ftypescript%2Flib%2Flib.es2015.iterable.d.ts%22%2C%22path%22%3A%22%2FApplications%2FVisual%20Studio%20Code.app%2FContents%2FResources%2Fapp%2Fextensions%2Fnode_modules%2Ftypescript%2Flib%2Flib.es2015.iterable.d.ts%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A215%2C%22character%22%3A0%7D%5D "../../../../../Applications/Visual Studio Code.app/Contents/Resources/app/extensions/node_modules/typescript/lib/lib.es2015.iterable.d.ts") that resolves to the configuration data if the fetch is successful, otherwise `null`.
   - **Pseudo Logic**:
     1. Attempt to fetch the `configuration.json` file using the [`fetch`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22path%22%3A%22%2FApplications%2FVisual%20Studio%20Code.app%2FContents%2FResources%2Fapp%2Fextensions%2Fnode_modules%2Ftypescript%2Flib%2Flib.dom.d.ts%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A28326%2C%22character%22%3A0%7D%5D "../../../../../Applications/Visual Studio Code.app/Contents/Resources/app/extensions/node_modules/typescript/lib/lib.dom.d.ts") API.
     2. If the response is OK, parse and return the JSON data.
     3. Catch any errors, log them, and return `null`.

3. **demoOptimiserData**
   - **Description**: Provides mock data for the optimiser report.
   - **Parameters**: None.
   - **Returns**: An object containing mock report data.
   - **Pseudo Logic**:
     1. Return a hardcoded object with mock data, including fields like [`ResourceName`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2Foptimiser.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2Foptimiser.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2Foptimiser.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A36%2C%22character%22%3A8%7D%5D "src/apis/optimiser.tsx"), [`ResourceStatus`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2Foptimiser.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2Foptimiser.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2Foptimiser.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A37%2C%22character%22%3A8%7D%5D "src/apis/optimiser.tsx"), [`LastUpdateDate`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2Foptimiser.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2Foptimiser.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2Foptimiser.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A38%2C%22character%22%3A8%7D%5D "src/apis/optimiser.tsx"), [`LastUpdateTiming`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2Foptimiser.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2Foptimiser.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2Foptimiser.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A39%2C%22character%22%3A8%7D%5D "src/apis/optimiser.tsx"), and [`CostSaving`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2Foptimiser.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2Foptimiser.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2Foptimiser.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A40%2C%22character%22%3A8%7D%5D "src/apis/optimiser.tsx").

### Example Usage

```tsx
// Example usage of getReport
getReport("policy123", "accountABC").then(report => {
  console.log(report);
});

// Example usage of getConfiguration
getConfiguration().then(config => {
  console.log(config

);


});
```

### Notes
- The `getReport` function currently returns mock data from `demoOptimiserData`. The commented-out code suggests that there are plans to implement an actual API call using `axios`.
- The `getConfiguration` function fetches configuration data from a local JSON file.
- The `demoOptimiserData` function provides a static mock response, useful for testing and development purposes.

sumitchohan: generate detailed documentation for current code file including all methods, fields, properties and summary of their implementation including pseudo logic

GitHub Copilot: ### File: [`policyDeployAPI.tsx`](command:_github.copilot.openRelativePath?%5B%7B%22scheme%22%3A%22file%22%2C%22authority%22%3A%22%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FpolicyDeployAPI.tsx%22%2C%22query%22%3A%22%22%2C%22fragment%22%3A%22%22%7D%5D "/Users/sumit/Downloads/Frontend/NAGISVPR/src/apis/policyDeployAPI.tsx")

#### Summary
This file contains two asynchronous functions: [`getConfiguration`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FpolicyDeployAPI.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FpolicyDeployAPI.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FpolicyDeployAPI.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A2%2C%22character%22%3A0%7D%5D "src/apis/policyDeployAPI.tsx") and [`policyDeployFunction`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FpolicyDeployAPI.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FpolicyDeployAPI.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FpolicyDeployAPI.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A16%2C%22character%22%3A0%7D%5D "src/apis/policyDeployAPI.tsx"). These functions are used to fetch configuration data and deploy a policy using an API, respectively.

---

### Function: [`getConfiguration`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FpolicyDeployAPI.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FpolicyDeployAPI.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FpolicyDeployAPI.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A2%2C%22character%22%3A0%7D%5D "src/apis/policyDeployAPI.tsx")

#### Description
Fetches the configuration data from a local [`configuration.json`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FpolicyDeployAPI.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FpolicyDeployAPI.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FpolicyDeployAPI.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A20%2C%22character%22%3A2%7D%5D "src/apis/policyDeployAPI.tsx") file.

#### Pseudo Logic
1. Attempt to fetch the [`configuration.json`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FpolicyDeployAPI.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FpolicyDeployAPI.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FpolicyDeployAPI.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A20%2C%22character%22%3A2%7D%5D "src/apis/policyDeployAPI.tsx") file using a GET request.
2. If the response is successful (status code 200), parse and return the JSON data.
3. If an error occurs during the fetch operation, log the error to the console.
4. Return `null` if the fetch operation fails or the response is not successful.

#### Implementation
```typescript
export const getConfiguration = async () => {
  try {
    const response = await fetch(`configuration.json`, {
      method: "GET",
    });
    if (response.ok) {
      return response.json();
    }
  } catch (error) {
    console.log(error);
  }
  return null;
};
```

---

### Function: `policyDeployFunction`

#### Parameters
- `accountId` (string): The ID of the account to which the policy will be deployed.
- `policyId` (string): The ID of the policy to be deployed.

#### Returns
- `Promise<Boolean>`: Returns `true` if the policy deployment is successful, otherwise returns `false`.

#### Description
Deploys a policy to a specified account using an API endpoint defined in the configuration data.

#### Pseudo Logic
1. Call `getConfiguration` to fetch the configuration data.
2. Extract the API URL for policy deployment from the configuration data.
3. Construct the request body with `PolicyId` and `AccountId`.
4. Attempt to send a POST request to the API endpoint with the constructed body.
5. If the response status is 200 and the response data's `statusCode` is 200, return `true`.
6. If an error occurs during the POST request, log the error to the console.
7. Return `false` if the POST request fails or the response is not successful.

#### Implementation
```typescript
export const policyDeployFunction = async (
  accountId: string,
  policyId: string
): Promise<Boolean> => {
  const configuration = await getConfiguration();
  const url = configuration.api.cloudpulse_policy_deploy;
  // const url =
  //   "https://i5v8p17fg9.execute-api.us-east-1.amazonaws.com/production/deploypolicy";
  const body = {
    PolicyId: policyId,
    AccountId: accountId,
  };
  try {
    const response = await axios.post(url, body);
    if (response?.status == 200) {
      if (response?.data?.statusCode == 200) {
        return true;
      }
    }
  } catch (error) {
    console.log(error);
  }
  return false;
};
```

---

### Fields and Properties
- **[`configuration.api.cloudpulse_policy_deploy`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FpolicyDeployAPI.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FpolicyDeployAPI.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FpolicyDeployAPI.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A20%2C%22character%22%3A2%7D%5D "src/apis/policyDeployAPI.tsx")**: The API endpoint URL for deploying policies, fetched from the configuration data.

### Error Handling
Both functions include basic error handling by logging errors to the console. This ensures that any issues during the fetch or POST request operations are recorded for debugging purposes.

sumitchohan: generate detailed documentation for current code file including all methods, fields, properties and summary of their implementation including pseudo logic

GitHub Copilot: ### File: [`policyGetEmail.tsx`](command:_github.copilot.openRelativePath?%5B%7B%22scheme%22%3A%22file%22%2C%22authority%22%3A%22%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FpolicyGetEmail.tsx%22%2C%22query%22%3A%22%22%2C%22fragment%22%3A%22%22%7D%5D "/Users/sumit/Downloads/Frontend/NAGISVPR/src/apis/policyGetEmail.tsx")

#### Summary
This file is currently empty and does not contain any methods, fields, properties, or implementations.

---

Since the file is empty, there are no functions, classes, or variables to document. If you add any code to this file, I can help generate detailed documentation for it.

sumitchohan: generate detailed documentation for current code file including all methods, fields, properties and summary of their implementation including pseudo logic

GitHub Copilot: ### File: [`policyPageAPIs.tsx`](command:_github.copilot.openRelativePath?%5B%7B%22scheme%22%3A%22file%22%2C%22authority%22%3A%22%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FpolicyPageAPIs.tsx%22%2C%22query%22%3A%22%22%2C%22fragment%22%3A%22%22%7D%5D "/Users/sumit/Downloads/Frontend/NAGISVPR/src/apis/policyPageAPIs.tsx")

#### Summary
This file contains two asynchronous functions: [`getConfiguration`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FpolicyPageAPIs.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FpolicyPageAPIs.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FpolicyPageAPIs.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A2%2C%22character%22%3A0%7D%5D "src/apis/policyPageAPIs.tsx") and [`updatePolicySchedule`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FpolicyPageAPIs.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FpolicyPageAPIs.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FpolicyPageAPIs.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A16%2C%22character%22%3A0%7D%5D "src/apis/policyPageAPIs.tsx"). These functions are used to fetch configuration data and update a policy schedule, respectively.

---

### Functions

#### [`getConfiguration`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FpolicyPageAPIs.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FpolicyPageAPIs.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FpolicyPageAPIs.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A2%2C%22character%22%3A0%7D%5D "src/apis/policyPageAPIs.tsx")

**Description:**
Fetches the configuration data from a [`configuration.json`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FpolicyPageAPIs.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FpolicyPageAPIs.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FpolicyPageAPIs.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A17%2C%22character%22%3A2%7D%5D "src/apis/policyPageAPIs.tsx") file.

**Parameters:**
- None

**Returns:**
- [`Promise<any>`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22external%22%3A%22file%3A%2F%2F%2FApplications%2FVisual%2520Studio%2520Code.app%2FContents%2FResources%2Fapp%2Fextensions%2Fnode_modules%2Ftypescript%2Flib%2Flib.es2015.iterable.d.ts%22%2C%22path%22%3A%22%2FApplications%2FVisual%20Studio%20Code.app%2FContents%2FResources%2Fapp%2Fextensions%2Fnode_modules%2Ftypescript%2Flib%2Flib.es2015.iterable.d.ts%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A215%2C%22character%22%3A0%7D%5D "../../../../../Applications/Visual Studio Code.app/Contents/Resources/app/extensions/node_modules/typescript/lib/lib.es2015.iterable.d.ts"): Returns a promise that resolves to the JSON content of the configuration file if the request is successful. Returns `null` if an error occurs or the response is not OK.

**Pseudo Logic:**
1. Try to fetch the [`configuration.json`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FpolicyPageAPIs.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FpolicyPageAPIs.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FpolicyPageAPIs.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A17%2C%22character%22%3A2%7D%5D "src/apis/policyPageAPIs.tsx") file using a GET request.
2. If the response is OK, parse and return the JSON content.
3. If an error occurs, log the error and return `null`.

**Code:**
```tsx
export const getConfiguration = async () => {
  try {
    const response = await fetch(`configuration.json`, {
      method: "GET",
    });
    if (response.ok) {
      return response.json();
    }
  } catch (error) {
    console.log(error);
  }
  return null;
};
```

#### `updatePolicySchedule`

**Description:**
Updates the policy schedule by sending a POST request to the API endpoint specified in the configuration.

**Parameters:**
- `body: any`: The request payload to be sent in the POST request.

**Returns:**
- `Promise<boolean>`: Returns a promise that resolves to `true` if the update is successful, otherwise `false`.

**Pseudo Logic:**
1. Fetch the configuration using `getConfiguration`.
2. Extract the API endpoint URL from the configuration.
3. Try to send a POST request to the extracted URL with the provided body.
4. If the response status is 200 and the response data's statusCode is 200, return `true`.
5. If an error occurs or the conditions are not met, log the error and return `false`.

**Code:**
```tsx
export const updatePolicySchedule = async (body: any): Promise<boolean> => {
  const configuration = await getConfiguration();
  const url = configuration.api.cloudpulse_update_policy_schedule;
  try {
    const response = await axios.post(url, body);
    if (response?.status == 200) {
      if (response?.data?.statusCode == 200) {
        return true;
      }
    }
  } catch (error) {
    console.log(error);
  }
  return false;
};
```

---

This documentation provides an overview of the functions, their parameters, return values, and the logic they implement.

sumitchohan: generate detailed documentation for current code file including all methods, fields, properties and summary of their implementation including pseudo logic

GitHub Copilot: ### File: [`policyPageEmail.tsx`](command:_github.copilot.openRelativePath?%5B%7B%22scheme%22%3A%22file%22%2C%22authority%22%3A%22%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FpolicyPageEmail.tsx%22%2C%22query%22%3A%22%22%2C%22fragment%22%3A%22%22%7D%5D "/Users/sumit/Downloads/Frontend/NAGISVPR/src/apis/policyPageEmail.tsx")

#### Summary
This file contains two asynchronous functions: [`getConfiguration`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FpolicyPageEmail.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FpolicyPageEmail.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FpolicyPageEmail.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A2%2C%22character%22%3A0%7D%5D "src/apis/policyPageEmail.tsx") and [`getPolicyEmail`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FpolicyPageEmail.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FpolicyPageEmail.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FpolicyPageEmail.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A16%2C%22character%22%3A0%7D%5D "src/apis/policyPageEmail.tsx"). These functions are used to fetch configuration data and retrieve policy email information, respectively.

---

### Functions

#### [`getConfiguration`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FpolicyPageEmail.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FpolicyPageEmail.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FpolicyPageEmail.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A2%2C%22character%22%3A0%7D%5D "src/apis/policyPageEmail.tsx")

**Description:**
Fetches the configuration data from a [`configuration.json`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FpolicyPageEmail.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FpolicyPageEmail.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FpolicyPageEmail.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A20%2C%22character%22%3A2%7D%5D "src/apis/policyPageEmail.tsx") file.

**Parameters:**
- None

**Returns:**
- [`Promise<any>`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22external%22%3A%22file%3A%2F%2F%2FApplications%2FVisual%2520Studio%2520Code.app%2FContents%2FResources%2Fapp%2Fextensions%2Fnode_modules%2Ftypescript%2Flib%2Flib.es2015.iterable.d.ts%22%2C%22path%22%3A%22%2FApplications%2FVisual%20Studio%20Code.app%2FContents%2FResources%2Fapp%2Fextensions%2Fnode_modules%2Ftypescript%2Flib%2Flib.es2015.iterable.d.ts%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A215%2C%22character%22%3A0%7D%5D "../../../../../Applications/Visual Studio Code.app/Contents/Resources/app/extensions/node_modules/typescript/lib/lib.es2015.iterable.d.ts"): Returns a promise that resolves to the JSON content of the configuration file if the request is successful. Returns `null` if an error occurs or the response is not OK.

**Pseudo Logic:**
1. Try to fetch the [`configuration.json`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FpolicyPageEmail.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FpolicyPageEmail.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FpolicyPageEmail.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A20%2C%22character%22%3A2%7D%5D "src/apis/policyPageEmail.tsx") file using a GET request.
2. If the response is OK, parse and return the JSON content.
3. If an error occurs, log the error and return `null`.

**Code:**
```tsx
export const getConfiguration = async () => {
  try {
    const response = await fetch(`configuration.json`, {
      method: "GET",
    });
    if (response.ok) {
      return response.json();
    }
  } catch (error) {
    console.log(error);
  }
  return null;
};
```

---

#### `getPolicyEmail`

**Description:**
Retrieves the policy email information by sending a POST request to the API endpoint specified in the configuration.

**Parameters:**
- `accountId: string`: The account ID for which the policy email is to be fetched.
- `policyId: string`: The policy ID for which the policy email is to be fetched.

**Returns:**
- `Promise<any>`: Returns a promise that resolves to the email address if the request is successful. Returns an empty string if an error occurs or the response is not OK.

**Pseudo Logic:**
1. Fetch the configuration using `getConfiguration`.
2. Extract the API endpoint URL from the configuration.
3. Construct the request payload with `accountId` and `policyId`.
4. Try to send a POST request to the extracted URL with the constructed payload.
5. If the response status is 200 and the response data's statusCode is 200, return the email address from the response data.
6. If an error occurs or the conditions are not met, log the error and return an empty string.

**Code:**
```tsx
export const getPolicyEmail = async (
  accountId: string,
  policyId: string
): Promise<any> => {
  const configuration = await getConfiguration();
  const url = configuration.api.cloudpulse_get_policy_email;
  // const url =
  //   "https://osnnlla9v4.execute-api.us-east-1.amazonaws.com/production/fetchemail";
  const body = {
    AccountId: accountId,
    PolicyId: policyId,
  };
  try {
    const response = await axios.post(url, body);
    if (response.status == 200) {
      if (response.data?.statusCode == 200) {
        return response.data?.body.email;
      }
    }
  } catch (error) {
    console.log(error);
  }
  return "";
};
```

---

This documentation provides an overview of the functions, their parameters, return values, and the logic they implement.

sumitchohan: generate detailed documentation for current code file including all methods, fields, properties and summary of their implementation including pseudo logic


GitHub Copilot: ### File: postCommentRI.tsx

#### Summary
This file contains a single function [`postCommentRI`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FpostCommentRI.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FpostCommentRI.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FpostCommentRI.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A2%2C%22character%22%3A0%7D%5D "src/apis/postCommentRI.tsx") which is responsible for posting a comment to a specified API endpoint using the Axios library. The function handles different types of errors that may occur during the HTTP request and logs appropriate error messages.

#### Functions

1. **postCommentRI**
   - **Description**: This asynchronous function sends a POST request to a specified API endpoint with a given payload. It handles and logs different types of errors that may occur during the request.
   - **Parameters**:
     - [`payload`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FpostCommentRI.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FpostCommentRI.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FpostCommentRI.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A2%2C%22character%22%3A36%7D%5D "src/apis/postCommentRI.tsx") (object): The data to be sent in the POST request.
   - **Returns**: A Promise that resolves to the response of the POST request.
   - **Pseudo Logic**:
     1. Log the payload to the console.
     2. Send a POST request to the API endpoint with the payload and a timeout of 5000 milliseconds.
     3. If the request is successful, return the response.
     4. If an error occurs:
        - If the error has a response, log the response data.
        - If the error has a request but no response, log the request.
        - If the error is a general error, log the error message.
     5. Rethrow the error to be caught by the caller.

#### Code
```tsx
import axios from "axios";

/**
 * Sends a POST request to the specified API endpoint with the given payload.
 * 
 * @param {object} payload - The data to be sent in the POST request.
 * @returns {Promise<any>} - A Promise that resolves to the response of the POST request.
 * @throws Will throw an error if the request fails.
 */
export const postCommentRI = async (payload: object): Promise<any> => {
  try {
    // Log the payload to

 the

 console
    console.log(payload);

    // Send a POST request to the API endpoint with the payload and a timeout of 5000 milliseconds
    const response = await axios.post(
      "https://n1ewphxcxc.execute-api.us-east-1.amazonaws.com/dev/push-comment",
      payload,
      {
        timeout: 5000,
      }
    );

    // Return the response if the request is successful
    return response;
  } catch (error) {
    // Handle different types of errors
    if (error.response) {
      // Log the response data if the error has a response
      console.error("Error Response:", error.response.data);
    } else if (error.request) {
      // Log the request if the error has a request but no response
      console.error("Error Request:", error.request);
    } else {
      // Log the error message if it is a general error
      console.error("Error Message:", error.message);
    }
    // Rethrow the error to be caught by the caller
    throw error;
  }
};
```

sumitchohan: generate detailed documentation for current code file including all methods, fields, properties and summary of their implementation including pseudo logic

GitHub Copilot: ### File: removeExceptionListAPI.tsx

#### Summary
This file contains a single function [`removeExceptionListAPI`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FremoveExceptionListAPI.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FremoveExceptionListAPI.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FremoveExceptionListAPI.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A16%2C%22character%22%3A0%7D%5D "src/apis/removeExceptionListAPI.tsx") which is responsible for sending a DELETE request to a specified API endpoint to remove an exception list. The function uses the Axios library to perform the HTTP request and handles any errors that may occur during the request.

#### Fields

1. **data**
   - **Type**: Object
   - **Description**: This object holds the default structure for the payload to be sent in the DELETE request. It includes fields for [`region`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FremoveExceptionListAPI.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FremoveExceptionListAPI.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FremoveExceptionListAPI.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A5%2C%22character%22%3A2%7D%5D "src/apis/removeExceptionListAPI.tsx"), [`accountid`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FremoveExceptionListAPI.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FremoveExceptionListAPI.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FremoveExceptionListAPI.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A6%2C%22character%22%3A2%7D%5D "src/apis/removeExceptionListAPI.tsx"), [`policyid`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FremoveExceptionListAPI.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FremoveExceptionListAPI.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FremoveExceptionListAPI.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A7%2C%22character%22%3A2%7D%5D "src/apis/removeExceptionListAPI.tsx"), and [`uniqueid`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FremoveExceptionListAPI.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FremoveExceptionListAPI.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FremoveExceptionListAPI.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A8%2C%22character%22%3A2%7D%5D "src/apis/removeExceptionListAPI.tsx").

#### Functions

1. **removeExceptionListAPI**
   - **Description**: This asynchronous function sends a DELETE request to a specified API endpoint with a given payload. It updates the [`data`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FremoveExceptionListAPI.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FremoveExceptionListAPI.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FremoveExceptionListAPI.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A4%2C%22character%22%3A0%7D%5D "src/apis/removeExceptionListAPI.tsx") object with the provided [`accountId`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FremoveExceptionListAPI.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FremoveExceptionListAPI.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FremoveExceptionListAPI.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A17%2C%22character%22%3A2%7D%5D "src/apis/removeExceptionListAPI.tsx"), [`policyId`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FremoveExceptionListAPI.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FremoveExceptionListAPI.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FremoveExceptionListAPI.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A18%2C%22character%22%3A2%7D%5D "src/apis/removeExceptionListAPI.tsx"), and [`uniqueID`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FremoveExceptionListAPI.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FremoveExceptionListAPI.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FremoveExceptionListAPI.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A19%2C%22character%22%3A2%7D%5D "src/apis/removeExceptionListAPI.tsx") before making the request.
   - **Parameters**:
     - [`accountId`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FremoveExceptionListAPI.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FremoveExceptionListAPI.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FremoveExceptionListAPI.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A17%2C%22character%22%3A2%7D%5D "src/apis/removeExceptionListAPI.tsx") (string): The account ID to be included in the payload.
     - [`policyId`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FremoveExceptionListAPI.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FremoveExceptionListAPI.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FremoveExceptionListAPI.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A18%2C%22character%22%3A2%7D%5D "src/apis/removeExceptionListAPI.tsx") (string): The policy ID to be included in the payload.
     - [`uniqueID`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FremoveExceptionListAPI.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FremoveExceptionListAPI.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FremoveExceptionListAPI.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A19%2C%22character%22%3A2%7D%5D "src/apis/removeExceptionListAPI.tsx") (Array): The unique ID(s) to be included in the payload.
   - **Returns**: A Promise that resolves to the response data of the DELETE request.
   - **Pseudo Logic**:
     1. Retrieve the configuration using [`getConfiguration`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FclientAPI.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A10%2C%22character%22%3A0%7D%5D "src/apis/clientAPI.tsx").
     2. Set the URL for the DELETE request (currently hardcoded).
     3. Update the [`data`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FremoveExceptionListAPI.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FremoveExceptionListAPI.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FremoveExceptionListAPI.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A4%2C%22character%22%3A0%7D%5D "src/apis/removeExceptionListAPI.tsx") object with the provided [`accountId`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FremoveExceptionListAPI.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FremoveExceptionListAPI.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FremoveExceptionListAPI.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A17%2C%22character%22%3A2%7D%5D "src/apis/removeExceptionListAPI.tsx"), [`policyId`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FremoveExceptionListAPI.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FremoveExceptionListAPI.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FremoveExceptionListAPI.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A18%2C%22character%22%3A2%7D%5D "src/apis/removeExceptionListAPI.tsx"), and [`uniqueID`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FremoveExceptionListAPI.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FremoveExceptionListAPI.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FremoveExceptionListAPI.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A19%2C%22character%22%3A2%7D%5D "src/apis/removeExceptionListAPI.tsx").
     4. Try to send a DELETE request to the API endpoint with the updated [`data`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FremoveExceptionListAPI.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FremoveExceptionListAPI.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FremoveExceptionListAPI.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A4%2C%22character%22%3A0%7D%5D "src/apis/removeExceptionListAPI.tsx") object.
     5. If the request is successful, log the response and return the response data.
     6. If an error occurs, log the error and return an empty array.

#### Code
```tsx
import axios from "axios";
import { API_BASE_URL } from "../utils/constants";
import { getConfiguration } from "./clientAPI";

// Default payload structure for the DELETE request
let data = {
  region: "*",
  accountid: "*",
  policyid: "*",
  uniqueid: [
    {
      unique_id: "*",
      Region: "*",
    },
  ],
};

/**
 * Sends a DELETE request to the specified API endpoint to remove an exception list.
 * 
 * @param {string} accountId - The account ID to be included in the payload.
 * @param {string} policyId - The policy ID to be included in the payload.
 * @param {Array} uniqueID - The unique ID(s) to be included in the payload.
 * @returns {Promise<any>} - A Promise that resolves to the response data of the DELETE request.
 * @throws Will throw an error if the request fails.
 */
export const removeExceptionListAPI = async (
  accountId: string,
  policyId: string,
  uniqueID: Array<any>
): Promise<any> => {
  // Retrieve the configuration
  const configuration = await getConfiguration();
  // Set the URL for the DELETE request (currently hardcoded)
  const url =
    "https://yptggcxn13.execute-api.us-east-1.amazonaws.com/dev/exception";
  
  // Update the data object with the provided parameters
  data.accountid = accountId;
  data.policyid = policyId;
  data.uniqueid = uniqueID;

  try {
    // Send a DELETE request to the API endpoint with the updated data object
    const response = await axios.delete(url, { data });
    // Log the response
    console.log(response);
    // Return the response data
    return response?.data;
  } catch (error) {
    // Log the error
    console.log(error);
    // Return an empty array in case of an error
    return [];
  }
};
```

sumitchohan: generate detailed documentation for current code file including all methods, fields, properties and summary of their implementation including pseudo logic

GitHub Copilot: ### Documentation for [`removeIgnoreListAPI.tsx`](command:_github.copilot.openRelativePath?%5B%7B%22scheme%22%3A%22file%22%2C%22authority%22%3A%22%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FremoveIgnoreListAPI.tsx%22%2C%22query%22%3A%22%22%2C%22fragment%22%3A%22%22%7D%5D "/Users/sumit/Downloads/Frontend/NAGISVPR/src/apis/removeIgnoreListAPI.tsx")

#### Summary
This file contains a function [`removeIgnoreListAPI`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FremoveIgnoreListAPI.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FremoveIgnoreListAPI.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FremoveIgnoreListAPI.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A16%2C%22character%22%3A0%7D%5D "src/apis/removeIgnoreListAPI.tsx") which is used to remove items from an ignore list by making an HTTP DELETE request to a specified API endpoint. The function uses Axios for making the HTTP request and retrieves configuration settings from another module.

#### Fields
- [`data`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FremoveIgnoreListAPI.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FremoveIgnoreListAPI.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FremoveIgnoreListAPI.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A4%2C%22character%22%3A0%7D%5D "src/apis/removeIgnoreListAPI.tsx"): An object that holds the request payload. It includes:
  - [`region`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FremoveIgnoreListAPI.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FremoveIgnoreListAPI.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FremoveIgnoreListAPI.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A5%2C%22character%22%3A2%7D%5D "src/apis/removeIgnoreListAPI.tsx"): A string representing the region.
  - [`accountid`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FremoveIgnoreListAPI.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FremoveIgnoreListAPI.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FremoveIgnoreListAPI.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A6%2C%22character%22%3A2%7D%5D "src/apis/removeIgnoreListAPI.tsx"): A string representing the account ID.
  - [`policyid`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FremoveIgnoreListAPI.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FremoveIgnoreListAPI.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FremoveIgnoreListAPI.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A7%2C%22character%22%3A2%7D%5D "src/apis/removeIgnoreListAPI.tsx"): A string representing the policy ID.
  - [`uniqueid`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FremoveIgnoreListAPI.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FremoveIgnoreListAPI.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FremoveIgnoreListAPI.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A8%2C%22character%22%3A2%7D%5D "src/apis/removeIgnoreListAPI.tsx"): An array of objects, each containing:
    - [`unique_id`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FremoveIgnoreListAPI.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FremoveIgnoreListAPI.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FremoveIgnoreListAPI.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A10%2C%22character%22%3A6%7D%5D "src/apis/removeIgnoreListAPI.tsx"): A string representing a unique identifier.
    - [`Region`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FremoveIgnoreListAPI.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FremoveIgnoreListAPI.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FremoveIgnoreListAPI.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A11%2C%22character%22%3A6%7D%5D "src/apis/removeIgnoreListAPI.tsx"): A string representing the region.

#### Methods

##### [`removeIgnoreListAPI`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FremoveIgnoreListAPI.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FremoveIgnoreListAPI.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FremoveIgnoreListAPI.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A16%2C%22character%22%3A0%7D%5D "src/apis/removeIgnoreListAPI.tsx")
```typescript
export const removeIgnoreListAPI = async (
  accountId: string,
  policyId: string,
  uniqueID: Array<{ unique_id: string; Region: string }>,
  reportRegion: string
): Promise<any> => { ... }
```

###### Parameters
- `accountId`: A string representing the account ID to be removed from the ignore list.
- `policyId`: A string representing the policy ID associated with the account.
- `uniqueID`: An array of objects, each containing:
  - `unique_id`: A string representing a unique identifier.
  - `Region`: A string representing the region.
- `reportRegion`: A string representing the region for the report.

###### Returns
- A promise that resolves to the response data from the API call or an empty array in case of an error.

###### Implementation
1. **Retrieve Configuration**: 
   - Call `getConfiguration` to get the configuration settings.
2. **Set URL**: 
   - Define the URL for the DELETE request. (Currently hardcoded but can be replaced with a dynamic URL from the configuration).
3. **Update Payload**: 
   - Update the `data` object with the provided parameters (`accountId`, `policyId`, `uniqueID`, `reportRegion`).
4. **Make HTTP Request**:
   - Use Axios to make an HTTP DELETE request to the specified URL with the `data` object as the payload.
5. **Handle Response**:
   - Log the response and return the response data.
6. **Handle Errors**:
   - Catch any errors, log them, and return an empty array.

#### Pseudo Logic
1. Import necessary modules (`axios`, `API_BASE_URL`, `getConfiguration`).
2. Define a `data` object with default values.
3. Export an asynchronous function `removeIgnoreListAPI` that:
   - Retrieves configuration settings.
   - Sets the URL for the API endpoint.
   - Updates the `data` object with provided parameters.
   - Makes an HTTP DELETE request using Axios.
   - Logs and returns the response data.
   - Catches and logs any errors, returning an empty array in case of an error.

#### Example Usage
```typescript
import { removeIgnoreListAPI } from './removeIgnoreListAPI';

const accountId = '12345';
const policyId = '67890';
const uniqueID = [{ unique_id: 'abcde', Region: 'us-east-1' }];
const reportRegion = 'us-west-2';

removeIgnoreListAPI(accountId, policyId, uniqueID, reportRegion)
  .then(response => {
    console.log('API Response:', response);
  })
  .catch(error => {
    console.error('Error:', error);
  });
```

This documentation provides a comprehensive overview of the [`removeIgnoreListAPI`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FremoveIgnoreListAPI.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FremoveIgnoreListAPI.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FremoveIgnoreListAPI.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A16%2C%22character%22%3A0%7D%5D "src/apis/removeIgnoreListAPI.tsx") function, its parameters, return values, and the logic it follows to perform its task.

sumitchohan: generate detailed documentation for current code file including all methods, fields, properties and summary of their implementation including pseudo logic

GitHub Copilot: ### Documentation for [`scheduleFetchAPI.tsx`](command:_github.copilot.openRelativePath?%5B%7B%22scheme%22%3A%22file%22%2C%22authority%22%3A%22%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FscheduleFetchAPI.tsx%22%2C%22query%22%3A%22%22%2C%22fragment%22%3A%22%22%7D%5D "/Users/sumit/Downloads/Frontend/NAGISVPR/src/apis/scheduleFetchAPI.tsx")

#### Summary
This file contains two main functions: [`getConfiguration`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FscheduleFetchAPI.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FscheduleFetchAPI.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FscheduleFetchAPI.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A7%2C%22character%22%3A0%7D%5D "src/apis/scheduleFetchAPI.tsx") and [`scheduleFetchAPI`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FscheduleFetchAPI.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FscheduleFetchAPI.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FscheduleFetchAPI.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A21%2C%22character%22%3A0%7D%5D "src/apis/scheduleFetchAPI.tsx"). These functions are used to fetch configuration data and to schedule an API call to fetch policy schedules, respectively.

#### Fields
- [`data`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FscheduleFetchAPI.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FscheduleFetchAPI.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FscheduleFetchAPI.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A2%2C%22character%22%3A0%7D%5D "src/apis/scheduleFetchAPI.tsx"): An object containing default values for [`PolicyId`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FscheduleFetchAPI.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FscheduleFetchAPI.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FscheduleFetchAPI.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A3%2C%22character%22%3A2%7D%5D "src/apis/scheduleFetchAPI.tsx") and [`AccountId`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FscheduleFetchAPI.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FscheduleFetchAPI.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FscheduleFetchAPI.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A4%2C%22character%22%3A2%7D%5D "src/apis/scheduleFetchAPI.tsx").

#### Methods

1. **getConfiguration**
   - **Description**: Fetches configuration data from a local JSON file.
   - **Parameters**: None
   - **Returns**: A promise that resolves to the configuration data if the fetch is successful, otherwise `null`.
   - **Pseudo Logic**:
     1. Attempt to fetch [`configuration.json`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FscheduleFetchAPI.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FscheduleFetchAPI.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FscheduleFetchAPI.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A22%2C%22character%22%3A2%7D%5D "src/apis/scheduleFetchAPI.tsx") using a GET request.
     2. If the response is successful, parse and return the JSON data.
     3. If an error occurs, log the error and return `null`.

2. **scheduleFetchAPI**
   - **Description**: Schedules an API call to fetch policy schedules using the provided [`policy_id`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FscheduleFetchAPI.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FscheduleFetchAPI.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FscheduleFetchAPI.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A21%2C%22character%22%3A39%7D%5D "src/apis/scheduleFetchAPI.tsx") and [`account_id`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FscheduleFetchAPI.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FscheduleFetchAPI.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FscheduleFetchAPI.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A21%2C%22character%22%3A50%7D%5D "src/apis/scheduleFetchAPI.tsx").
   - **Parameters**:
     - [`policy_id`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FscheduleFetchAPI.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FscheduleFetchAPI.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FscheduleFetchAPI.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A21%2C%22character%22%3A39%7D%5D "src/apis/scheduleFetchAPI.tsx"): The ID of the policy to fetch the schedule for.
     - [`account_id`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FscheduleFetchAPI.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FscheduleFetchAPI.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FscheduleFetchAPI.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A21%2C%22character%22%3A50%7D%5D "src/apis/scheduleFetchAPI.tsx"): The ID of the account associated with the policy.
   - **Returns**: A promise that resolves to the response data from the API call, or an empty array if an error occurs.
   - **Pseudo Logic**:
     1. Call [`getConfiguration`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FscheduleFetchAPI.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FscheduleFetchAPI.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FscheduleFetchAPI.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A7%2C%22character%22%3A0%7D%5D "src/apis/scheduleFetchAPI.tsx") to fetch the configuration data.
     2. Extract the API URL from the configuration data.
     3. Update the [`data`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FscheduleFetchAPI.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FscheduleFetchAPI.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FscheduleFetchAPI.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A2%2C%22character%22%3A0%7D%5D "src/apis/scheduleFetchAPI.tsx") object with the provided [`policy_id`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FscheduleFetchAPI.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FscheduleFetchAPI.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FscheduleFetchAPI.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A21%2C%22character%22%3A39%7D%5D "src/apis/scheduleFetchAPI.tsx") and [`account_id`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FscheduleFetchAPI.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FscheduleFetchAPI.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FscheduleFetchAPI.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A21%2C%22character%22%3A50%7D%5D "src/apis/scheduleFetchAPI.tsx").
     4. Attempt to make a POST request to the API URL with the updated [`data`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FscheduleFetchAPI.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FscheduleFetchAPI.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FscheduleFetchAPI.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A2%2C%22character%22%3A0%7D%5D "src/apis/scheduleFetchAPI.tsx") object.
     5. If the request is successful, return the response data.
     6. If an error occurs, log the error and return an empty array.

#### Code

```tsx
import axios from "axios";

let data = {
  PolicyId: "CO1011",
  AccountId: "515302636750",
};

/**
 * Fetches configuration data from a local JSON file.
 * @returns {Promise<any>} A promise that resolves to the configuration data if successful, otherwise null.
 */
export const getConfiguration = async () => {
  try {
    const response = await fetch(`configuration.json`, {
      method: "GET",
    });
    if (response.ok) {
      return response.json();
    }
  } catch (error) {
    console.log(error);
  }
  return null;
};

/**
 * Schedules an API call to fetch policy schedules using the provided policy_id and account_id.
 * @param {string} policy_id - The ID of the policy to fetch the schedule for.
 * @param {string} account_id - The ID of the account associated with the policy.
 * @returns {Promise<any>} A promise that resolves to the response data from the API call, or an empty array if an error occurs.
 */
export const scheduleFetchAPI = async (policy_id, account_id): Promise<any> => {
  const configuration = await getConfiguration();
  const url = configuration.api.cloudpulse_fetch_policy_schedule;
  // const url =
  //   "https://zew9oe8mk4.execute-api.us-east-1.amazonaws.com/production/fetchschedule";
  data.AccountId = account_id;
  data.PolicyId = policy_id;
  try {
    const response = await axios.post(url, data);
    // console.log(response?.data);
    return response?.data;
  } catch (error) {
    console.log(error);
    return [];
  }
};
```

sumitchohan: generate detailed documentation for current code file including all methods, fields, properties and summary of their implementation including pseudo logic

GitHub Copilot: ### Documentation for [`accountAlias.tsx`](command:_github.copilot.openRelativePath?%5B%7B%22scheme%22%3A%22file%22%2C%22authority%22%3A%22%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FaccountAlias.tsx%22%2C%22query%22%3A%22%22%2C%22fragment%22%3A%22%22%7D%5D "/Users/sumit/Downloads/Frontend/NAGISVPR/src/components/AwsFormComponents/accountAlias.tsx")

#### Summary
This file defines a React functional component [`AccountAlias`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FaccountAlias.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FaccountAlias.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FaccountAlias.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A23%2C%22character%22%3A0%7D%5D "src/components/AwsFormComponents/accountAlias.tsx") which is used to manage and validate account aliases. It includes form controls for user input, validation logic, and API calls to fetch existing account aliases.

#### Imports
- **FontAwesomeIcon**: Used for rendering icons.
- **Material UI Components**: [`FormControl`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FaccountAlias.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FaccountAlias.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FaccountAlias.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A2%2C%22character%22%3A0%7D%5D "src/components/AwsFormComponents/accountAlias.tsx"), [`FormControlLabel`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FaccountAlias.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FaccountAlias.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FaccountAlias.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A2%2C%22character%22%3A0%7D%5D "src/components/AwsFormComponents/accountAlias.tsx"), [`InputAdornment`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FaccountAlias.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FaccountAlias.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FaccountAlias.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A2%2C%22character%22%3A0%7D%5D "src/components/AwsFormComponents/accountAlias.tsx"), [`Radio`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FaccountAlias.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FaccountAlias.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FaccountAlias.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A2%2C%22character%22%3A0%7D%5D "src/components/AwsFormComponents/accountAlias.tsx"), [`RadioGroup`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FaccountAlias.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FaccountAlias.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FaccountAlias.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A2%2C%22character%22%3A0%7D%5D "src/components/AwsFormComponents/accountAlias.tsx"), [`TextField`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FaccountAlias.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FaccountAlias.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FaccountAlias.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A2%2C%22character%22%3A0%7D%5D "src/components/AwsFormComponents/accountAlias.tsx") for form elements.
- **React Hooks**: [`useEffect`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FaccountAlias.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FaccountAlias.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FaccountAlias.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A10%2C%22character%22%3A0%7D%5D "src/components/AwsFormComponents/accountAlias.tsx"), [`useState`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FaccountAlias.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FaccountAlias.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FaccountAlias.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A10%2C%22character%22%3A0%7D%5D "src/components/AwsFormComponents/accountAlias.tsx") for managing component state and side effects.
- **toast**: For displaying notifications.
- **getAllAccounts**: API call to fetch account data.

#### Interfaces
- **accountAliasProps**
  - [`onNext`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FaccountAlias.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FaccountAlias.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FaccountAlias.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A15%2C%22character%22%3A2%7D%5D "src/components/AwsFormComponents/accountAlias.tsx"): Function to handle the next step.
  - [`onPrev`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FaccountAlias.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FaccountAlias.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FaccountAlias.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A16%2C%22character%22%3A2%7D%5D "src/components/AwsFormComponents/accountAlias.tsx"): Function to handle the previous step.
  - [`onData`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FaccountAlias.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FaccountAlias.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FaccountAlias.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A17%2C%22character%22%3A2%7D%5D "src/components/AwsFormComponents/accountAlias.tsx"): Function to handle data submission.
  - [`initialAlias`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FaccountAlias.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FaccountAlias.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FaccountAlias.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A18%2C%22character%22%3A2%7D%5D "src/components/AwsFormComponents/accountAlias.tsx") (optional): Initial alias value.
  - [`formData`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FaccountAlias.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FaccountAlias.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FaccountAlias.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A19%2C%22character%22%3A2%7D%5D "src/components/AwsFormComponents/accountAlias.tsx"): Object containing form data with an [`alias`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FaccountAlias.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FaccountAlias.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FaccountAlias.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A19%2C%22character%22%3A14%7D%5D "src/components/AwsFormComponents/accountAlias.tsx") field.

#### Component: [`AccountAlias`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FaccountAlias.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FaccountAlias.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FaccountAlias.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A23%2C%22character%22%3A0%7D%5D "src/components/AwsFormComponents/accountAlias.tsx")
- **Props**: 
  - [`onNext`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FaccountAlias.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FaccountAlias.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FaccountAlias.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A15%2C%22character%22%3A2%7D%5D "src/components/AwsFormComponents/accountAlias.tsx"), [`onPrev`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FaccountAlias.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FaccountAlias.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FaccountAlias.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A16%2C%22character%22%3A2%7D%5D "src/components/AwsFormComponents/accountAlias.tsx"), [`onData`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FaccountAlias.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FaccountAlias.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FaccountAlias.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A17%2C%22character%22%3A2%7D%5D "src/components/AwsFormComponents/accountAlias.tsx"), [`initialAlias`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FaccountAlias.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FaccountAlias.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FaccountAlias.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A18%2C%22character%22%3A2%7D%5D "src/components/AwsFormComponents/accountAlias.tsx"), [`formData`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FaccountAlias.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FaccountAlias.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FaccountAlias.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A19%2C%22character%22%3A2%7D%5D "src/components/AwsFormComponents/accountAlias.tsx")
- **State**:
  - [`accountAlias`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FaccountAlias.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FaccountAlias.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FaccountAlias.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A30%2C%22character%22%3A2%7D%5D "src/components/AwsFormComponents/accountAlias.tsx"): Stores the current alias input by the user.
  - [`isValid`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FaccountAlias.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FaccountAlias.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FaccountAlias.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A33%2C%22character%22%3A2%7D%5D "src/components/AwsFormComponents/accountAlias.tsx"): Boolean to track if the alias is valid.
  - [`existingAliases`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FaccountAlias.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FaccountAlias.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FaccountAlias.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A35%2C%22character%22%3A2%7D%5D "src/components/AwsFormComponents/accountAlias.tsx"): Stores the list of existing aliases fetched from the API.

#### Pseudo Logic
1. **Initialization**:
   - Initialize [`accountAlias`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FaccountAlias.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FaccountAlias.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FaccountAlias.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A30%2C%22character%22%3A2%7D%5D "src/components/AwsFormComponents/accountAlias.tsx") with [`formData.alias`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FaccountAlias.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FaccountAlias.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FaccountAlias.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A19%2C%22character%22%3A2%7D%5D "src/components/AwsFormComponents/accountAlias.tsx") or an empty string.
   - Initialize [`isValid`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FaccountAlias.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FaccountAlias.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FaccountAlias.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A33%2C%22character%22%3A2%7D%5D "src/components/AwsFormComponents/accountAlias.tsx") to `true`.
   - Initialize [`existingAliases`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FaccountAlias.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FaccountAlias.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FaccountAlias.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A35%2C%22character%22%3A2%7D%5D "src/components/AwsFormComponents/accountAlias.tsx") to an empty state.

2. **Effect Hook**:
   - Define an asynchronous function [`fetchData`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FaccountAlias.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FaccountAlias.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FaccountAlias.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A38%2C%22character%22%3A4%7D%5D "src/components/AwsFormComponents/accountAlias.tsx") to fetch existing account aliases.
   - Use `try-catch` to handle API call errors.
   - Call [`getAllAccounts`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FaccountAPI.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A51%2C%22character%22%3A0%7D%5D "src/apis/accountAPI.tsx") API to fetch data.
   - Store the fetched data in [`existingAliases`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FaccountAlias.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FaccountAlias.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FaccountAlias.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A35%2C%22character%22%3A2%7D%5D "src/components/AwsFormComponents/accountAlias.tsx").

3. **Rendering**:
   - Render form controls including [`TextField`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FaccountAlias.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FaccountAlias.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FaccountAlias.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A2%2C%22character%22%3A0%7D%5D "src/components/AwsFormComponents/accountAlias.tsx") for alias input.
   - Use [`InputAdornment`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FaccountAlias.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FaccountAlias.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FaccountAlias.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A2%2C%22character%22%3A0%7D%5D "src/components/AwsFormComponents/accountAlias.tsx") to add an icon inside the input field.
   - Render [`RadioGroup`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FaccountAlias.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FaccountAlias.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FaccountAlias.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A2%2C%22character%22%3A0%7D%5D "src/components/AwsFormComponents/accountAlias.tsx") for additional options if needed.
   - Add event handlers for form controls to update state and validate input.

#### Code
```tsx
import { faMagnifyingGlass } from "@fortawesome/free-solid-svg-icons";
import { FontAwesomeIcon } from "@fortawesome/react-fontawesome";
import {
  FormControl,
  FormControlLabel,
  InputAdornment,
  Radio,
  RadioGroup,
  TextField,
} from "@mui/material";
import { useEffect, useState } from "react";
import toast from "react-hot-toast";
import { getAllAccounts } from "../../apis/accountAPI";

interface accountAliasProps {
  onNext: () => void;
  onPrev: () => void;
  onData: (aliasData: string) => void;
  initialAlias?: string;
  formData: { alias: string };
}

const AccountAlias: React.FC<accountAliasProps> = ({
  onNext,
  onPrev,
  onData,
  initialAlias = "",
  formData,
}) => {
  const [accountAlias, setAccountAlias] = useState<string>(
    formData.alias || ""
  );
  const [isValid, setIsValid] = useState<boolean>(true);
  const [existingAliases, setExistingAliases] = useState<any>();

  useEffect(() => {
    const fetchData = async () => {
      try {
        // const result = await getAllAccounts("UniqueId", "CloudProvider");
        // setExistingAliases(result);
      } catch (error) {
        toast.error("Failed to fetch account aliases");
      }
    };

    fetchData();
  }, []);

  return (
    <FormControl>
      <TextField
        label="Account Alias"
        value={accountAlias}
        onChange={(e) => setAccountAlias(e.target.value)}
        InputProps={{
          startAdornment: (
            <InputAdornment position="start">
              <FontAwesomeIcon icon={faMagnifyingGlass} />
            </InputAdornment>
          ),
        }}
      />
      <RadioGroup>
        <FormControlLabel value="option1" control={<Radio />} label="Option 1" />
        <FormControlLabel value="option2" control={<Radio />} label="Option 2" />
      </RadioGroup>
    </FormControl>
  );
};

export default AccountAlias;
```

This documentation provides a detailed overview of the `AccountAlias` component, including its purpose, props, state, and the logic implemented within the component.

sumitchohan: generate detailed documentation for current code file including all methods, fields, properties and summary of their implementation including pseudo logic

GitHub Copilot: ### File: [`awsConsole.tsx`](command:_github.copilot.openRelativePath?%5B%7B%22scheme%22%3A%22file%22%2C%22authority%22%3A%22%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FawsConsole.tsx%22%2C%22query%22%3A%22%22%2C%22fragment%22%3A%22%22%7D%5D "/Users/sumit/Downloads/Frontend/NAGISVPR/src/components/AwsFormComponents/awsConsole.tsx")

#### Summary
This file defines a React functional component named [`AwsConsole`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FawsConsole.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FawsConsole.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FawsConsole.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A18%2C%22character%22%3A0%7D%5D "src/components/AwsFormComponents/awsConsole.tsx"). The component interacts with AWS services and provides functionalities such as copying data to the clipboard and fetching configuration details. It uses several hooks to manage state and side effects.

#### Imports
- **[`unicode`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FawsConsole.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FawsConsole.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FawsConsole.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A0%2C%22character%22%3A0%7D%5D "src/components/AwsFormComponents/awsConsole.tsx")**: Imported from `@fortawesome/free-regular-svg-icons/faAddressBook`.
- **[`React`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FawsConsole.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FawsConsole.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FawsConsole.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A1%2C%22character%22%3A0%7D%5D "src/components/AwsFormComponents/awsConsole.tsx")**: Core React library for building the component.
- **[`useEffect`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FawsConsole.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FawsConsole.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FawsConsole.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A1%2C%22character%22%3A0%7D%5D "src/components/AwsFormComponents/awsConsole.tsx"), [`useState`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FawsConsole.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FawsConsole.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FawsConsole.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A1%2C%22character%22%3A0%7D%5D "src/components/AwsFormComponents/awsConsole.tsx")**: React hooks for managing side effects and state.
- **[`CopyToClipboard`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FawsConsole.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FawsConsole.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FawsConsole.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A2%2C%22character%22%3A0%7D%5D "src/components/AwsFormComponents/awsConsole.tsx")**: A component for copying text to the clipboard.
- **[`createAccountStackCheck`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FcreateAccountCheckStack.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A20%2C%22character%22%3A0%7D%5D "src/apis/createAccountCheckStack.tsx")**: API function for checking account stack.
- **[`create`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FawsConsole.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FawsConsole.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FawsConsole.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A4%2C%22character%22%3A0%7D%5D "src/components/AwsFormComponents/awsConsole.tsx")**: Imported from `@mui/material/styles/createTransitions`.
- **[`useNavigate`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FawsConsole.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FawsConsole.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FawsConsole.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A5%2C%22character%22%3A0%7D%5D "src/components/AwsFormComponents/awsConsole.tsx")**: Hook from `react-router-dom` for navigation.
- **[`getRegistedUser`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fservices%2FauthService.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A20%2C%22character%22%3A0%7D%5D "src/services/authService.tsx")**: Service function to get registered user details.
- **[`User`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Futils%2Finterfaces.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A50%2C%22character%22%3A0%7D%5D "src/utils/interfaces.tsx")**: Interface defining the structure of a user object.
- **[`getConfiguration`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FloginAPI.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FloginAPI.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FloginAPI.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A3%2C%22character%22%3A0%7D%5D "src/apis/loginAPI.tsx")**: API function to get configuration details.

#### Interfaces
- **[`AwsConsoleProps`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FawsConsole.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FawsConsole.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FawsConsole.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A10%2C%22character%22%3A0%7D%5D "src/components/AwsFormComponents/awsConsole.tsx")**: Defines the properties that the [`AwsConsole`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FawsConsole.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FawsConsole.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FawsConsole.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A18%2C%22character%22%3A0%7D%5D "src/components/AwsFormComponents/awsConsole.tsx") component accepts.
  - [`onCopy?`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FawsConsole.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FawsConsole.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FawsConsole.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A11%2C%22character%22%3A2%7D%5D "src/components/AwsFormComponents/awsConsole.tsx"): Optional callback function to be called when data is copied.
  - [`uniqueId?`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FawsConsole.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FawsConsole.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FawsConsole.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A12%2C%22character%22%3A2%7D%5D "src/components/AwsFormComponents/awsConsole.tsx"): Optional unique identifier.
  - [`externalId?`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FawsConsole.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FawsConsole.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FawsConsole.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A13%2C%22character%22%3A2%7D%5D "src/components/AwsFormComponents/awsConsole.tsx"): Optional external identifier.
  - [`onPrev?`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FawsConsole.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FawsConsole.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FawsConsole.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A14%2C%22character%22%3A2%7D%5D "src/components/AwsFormComponents/awsConsole.tsx"): Optional callback function for previous action.
  - [`switchData`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FawsConsole.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FawsConsole.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FawsConsole.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A15%2C%22character%22%3A2%7D%5D "src/components/AwsFormComponents/awsConsole.tsx"): Function to switch data, takes a boolean and a number as arguments.

#### Component: [`AwsConsole`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FawsConsole.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FawsConsole.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FawsConsole.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A18%2C%22character%22%3A0%7D%5D "src/components/AwsFormComponents/awsConsole.tsx")
- **Props**: Accepts [`AwsConsoleProps`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FawsConsole.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FawsConsole.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FawsConsole.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A10%2C%22character%22%3A0%7D%5D "src/components/AwsFormComponents/awsConsole.tsx").
- **State Variables**:
  - [`copied`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FawsConsole.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FawsConsole.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FawsConsole.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A19%2C%22character%22%3A2%7D%5D "src/components/AwsFormComponents/awsConsole.tsx"): Boolean state to track if data has been copied.
  - [`stackEnable`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FawsConsole.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FawsConsole.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FawsConsole.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A20%2C%22character%22%3A2%7D%5D "src/components/AwsFormComponents/awsConsole.tsx"): Boolean state to track if the stack is enabled.
  - [`templateUrl`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FawsConsole.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FawsConsole.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FawsConsole.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A21%2C%22character%22%3A2%7D%5D "src/components/AwsFormComponents/awsConsole.tsx"): String state to store the template URL.
  - [`accountId`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FawsConsole.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FawsConsole.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FawsConsole.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A22%2C%22character%22%3A2%7D%5D "src/components/AwsFormComponents/awsConsole.tsx"): String state to store the account ID.

#### Hooks
- **[`useEffect`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FawsConsole.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FawsConsole.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FawsConsole.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A1%2C%22character%22%3A0%7D%5D "src/components/AwsFormComponents/awsConsole.tsx")**: Fetches configuration details when the component mounts and updates [`templateUrl`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FawsConsole.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FawsConsole.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FawsConsole.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A21%2C%22character%22%3A2%7D%5D "src/components/AwsFormComponents/awsConsole.tsx") and [`accountId`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FawsConsole.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FawsConsole.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FawsConsole.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A22%2C%22character%22%3A2%7D%5D "src/components/AwsFormComponents/awsConsole.tsx") state variables.
  - **Pseudo Logic**:
    1. Define an asynchronous function.
    2. Call [`getConfiguration`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FloginAPI.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FloginAPI.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FloginAPI.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A3%2C%22character%22%3A0%7D%5D "src/apis/loginAPI.tsx") to fetch configuration details.
    3. Update [`templateUrl`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FawsConsole.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FawsConsole.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FawsConsole.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A21%2C%22character%22%3A2%7D%5D "src/components/AwsFormComponents/awsConsole.tsx") and [`accountId`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FawsConsole.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FawsConsole.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FawsConsole.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A22%2C%22character%22%3A2%7D%5D "src/components/AwsFormComponents/awsConsole.tsx") with the fetched data.

#### Methods
- **[`handleCopy`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FawsConsole.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FawsConsole.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FawsConsole.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A32%2C%22character%22%3A2%7D%5D "src/components/AwsFormComponents/awsConsole.tsx")**: Handles the copy action.
  - **Pseudo Logic**:
    1. Set [`copied`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FawsConsole.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FawsConsole.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FawsConsole.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A19%2C%22character%22%3A2%7D%5D "src/components/AwsFormComponents/awsConsole.tsx") state to `true`.
    2. Call the optional [`onCopy`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FawsConsole.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FawsConsole.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FawsConsole.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A11%2C%22character%22%3A2%7D%5D "src/components/AwsFormComponents/awsConsole.tsx") callback if provided.

#### Additional Code
- **[`currentDate`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FawsConsole.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FawsConsole.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FawsConsole.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A37%2C%22character%22%3A2%7D%5D "src/components/AwsFormComponents/awsConsole.tsx")**: A [`Date`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22external%22%3A%22file%3A%2F%2F%2FApplications%2FVisual%2520Studio%2520Code.app%2FContents%2FResources%2Fapp%2Fextensions%2Fnode_modules%2Ftypescript%2Flib%2Flib.es2015.symbol.wellknown.d.ts%22%2C%22path%22%3A%22%2FApplications%2FVisual%20Studio%20Code.app%2FContents%2FResources%2Fapp%2Fextensions%2Fnode_modules%2Ftypescript%2Flib%2Flib.es2015.symbol.wellknown.d.ts%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A111%2C%22character%22%3A0%7D%5D "../../../../../Applications/Visual Studio Code.app/Contents/Resources/app/extensions/node_modules/typescript/lib/lib.es2015.symbol.wellknown.d.ts") object representing the current date.
- **Date Formatting**: Extracts the year from the [`currentDate`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FawsConsole.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FawsConsole.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FawsConsole.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A37%2C%22character%22%3A2%7D%5D "src/components/AwsFormComponents/awsConsole.tsx").

### Pseudo Logic for [`AwsConsole`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FawsConsole.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FawsConsole.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FawsConsole.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A18%2C%22character%22%3A0%7D%5D "src/components/AwsFormComponents/awsConsole.tsx") Component
1. **Initialize State Variables**:
   - [`copied`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FawsConsole.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FawsConsole.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FawsConsole.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A19%2C%22character%22%3A2%7D%5D "src/components/AwsFormComponents/awsConsole.tsx") to `false`.
   - [`stackEnable`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FawsConsole.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FawsConsole.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FawsConsole.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A20%2C%22character%22%3A2%7D%5D "src/components/AwsFormComponents/awsConsole.tsx") to `false`.
   - [`templateUrl`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FawsConsole.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FawsConsole.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FawsConsole.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A21%2C%22character%22%3A2%7D%5D "src/components/AwsFormComponents/awsConsole.tsx") to an empty string.
   - [`accountId`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FawsConsole.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FawsConsole.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FawsConsole.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A22%2C%22character%22%3A2%7D%5D "src/components/AwsFormComponents/awsConsole.tsx") to an empty string.

2. **Fetch Configuration on Mount**:
   - Use [`useEffect`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FawsConsole.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FawsConsole.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FawsConsole.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A1%2C%22character%22%3A0%7D%5D "src/components/AwsFormComponents/awsConsole.tsx") to run an asynchronous function on component mount.
   - Fetch configuration using [`getConfiguration`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FloginAPI.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FloginAPI.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FloginAPI.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A3%2C%22character%22%3A0%7D%5D "src/apis/loginAPI.tsx").
   - Update [`templateUrl`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FawsConsole.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FawsConsole.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FawsConsole.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A21%2C%22character%22%3A2%7D%5D "src/components/AwsFormComponents/awsConsole.tsx") and [`accountId`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FawsConsole.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FawsConsole.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FawsConsole.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A22%2C%22character%22%3A2%7D%5D "src/components/AwsFormComponents/awsConsole.tsx") with the fetched data.

3. **Handle Copy Action**:
   - Define [`handleCopy`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FawsConsole.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FawsConsole.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FawsConsole.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A32%2C%22character%22%3A2%7D%5D "src/components/AwsFormComponents/awsConsole.tsx") function.
   - Set [`copied`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FawsConsole.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FawsConsole.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FawsConsole.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A19%2C%22character%22%3A2%7D%5D "src/components/AwsFormComponents/awsConsole.tsx") state to `true`.
   - Call [`onCopy`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FawsConsole.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FawsConsole.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FawsConsole.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A11%2C%22character%22%3A2%7D%5D "src/components/AwsFormComponents/awsConsole.tsx") callback if provided.

4. **Date Handling**:
   - Create a [`Date`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22external%22%3A%22file%3A%2F%2F%2FApplications%2FVisual%2520Studio%2520Code.app%2FContents%2FResources%2Fapp%2Fextensions%2Fnode_modules%2Ftypescript%2Flib%2Flib.es2015.symbol.wellknown.d.ts%22%2C%22path%22%3A%22%2FApplications%2FVisual%20Studio%20Code.app%2FContents%2FResources%2Fapp%2Fextensions%2Fnode_modules%2Ftypescript%2Flib%2Flib.es2015.symbol.wellknown.d.ts%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A111%2C%22character%22%3A0%7D%5D "../../../../../Applications/Visual Studio Code.app/Contents/Resources/app/extensions/node_modules/typescript/lib/lib.es2015.symbol.wellknown.d.ts") object for the current date.
   - Extract the year from the [`Date`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22external%22%3A%22file%3A%2F%2F%2FApplications%2FVisual%2520Studio%2520Code.app%2FContents%2FResources%2Fapp%2Fextensions%2Fnode_modules%2Ftypescript%2Flib%2Flib.es2015.symbol.wellknown.d.ts%22%2C%22path%22%3A%22%2FApplications%2FVisual%20Studio%20Code.app%2FContents%2FResources%2Fapp%2Fextensions%2Fnode_modules%2Ftypescript%2Flib%2Flib.es2015.symbol.wellknown.d.ts%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A111%2C%22character%22%3A0%7D%5D "../../../../../Applications/Visual Studio Code.app/Contents/Resources/app/extensions/node_modules/typescript/lib/lib.es2015.symbol.wellknown.d.ts") object.

### Code
```tsx
import { unicode } from "@fortawesome/free-regular-svg-icons/faAddressBook";
import React, { useEffect, useState } from "react";
import { CopyToClipboard } from "react-copy-to-clipboard";
import { createAccountStackCheck } from "../../apis/createAccountCheckStack";
import { create } from "@mui/material/styles/createTransitions";
import { useNavigate } from "react-router-dom";
import { getRegistedUser } from "../../services/authService";
import { User } from "../../utils/interfaces";
import { getConfiguration } from "../../apis/loginAPI";

interface AwsConsoleProps {
  onCopy?: () => void;
  uniqueId?: string;
  externalId?: string;
  onPrev?: () => void;
  switchData: (boolData: boolean, swithNumber: number) => void;
}

const AwsConsole: React.FC<AwsConsoleProps> = (props, { onCopy }) => {
  const [copied, setCopied] = useState(false);
  const [stackEnable, setStackEnable] = useState(false);
  const [templateUrl, setTemplateUrl] = useState("");
  const [accountId, setAccountID] = useState("");

  useEffect(() => {
    void (async () => {
      const configuration = await getConfiguration();
      setTemplateUrl(configuration.stack.aws_console_template_url);
      setAccountID(configuration.stack.aws_source_account_id);
    })();
  }, []);

  const handleCopy = () => {
    setCopied(true);
    onCopy?.();
  };

  const currentDate = new Date();
  const year = currentDate.getFullYear();
};
```

sumitchohan: generate detailed documentation for current code file including all methods, fields, properties and summary of their implementation including pseudo logic


GitHub Copilot: The current file [`createStack.tsx`](command:_github.copilot.openRelativePath?%5B%7B%22scheme%22%3A%22file%22%2C%22authority%22%3A%22%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FcreateStack.tsx%22%2C%22query%22%3A%22%22%2C%22fragment%22%3A%22%22%7D%5D "/Users/sumit/Downloads/Frontend/NAGISVPR/src/components/AwsFormComponents/createStack.tsx") is empty. To generate detailed documentation, we need to have some code in the file. Please add some code or let me know what functionality you want to implement in this file, and I can help you with that.

sumitchohan: generate detailed documentation for current code file including all methods, fields, properties and summary of their implementation including pseudo logic

GitHub Copilot: ### Documentation for [`enviromentAlias.tsx`](command:_github.copilot.openRelativePath?%5B%7B%22scheme%22%3A%22file%22%2C%22authority%22%3A%22%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FenviromentAlias.tsx%22%2C%22query%22%3A%22%22%2C%22fragment%22%3A%22%22%7D%5D "/Users/sumit/Downloads/Frontend/NAGISVPR/src/components/AwsFormComponents/enviromentAlias.tsx")

#### Imports
- **[`faMagnifyingGlass`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FenviromentAlias.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FenviromentAlias.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FenviromentAlias.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A0%2C%22character%22%3A0%7D%5D "src/components/AwsFormComponents/enviromentAlias.tsx")**: Icon from FontAwesome.
- **[`FontAwesomeIcon`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FenviromentAlias.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FenviromentAlias.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FenviromentAlias.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A1%2C%22character%22%3A0%7D%5D "src/components/AwsFormComponents/enviromentAlias.tsx")**: Component to render FontAwesome icons.
- **[`FormControl`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FenviromentAlias.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FenviromentAlias.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FenviromentAlias.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A2%2C%22character%22%3A0%7D%5D "src/components/AwsFormComponents/enviromentAlias.tsx"), [`FormControlLabel`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FenviromentAlias.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FenviromentAlias.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FenviromentAlias.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A2%2C%22character%22%3A0%7D%5D "src/components/AwsFormComponents/enviromentAlias.tsx"), [`InputAdornment`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FenviromentAlias.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FenviromentAlias.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FenviromentAlias.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A2%2C%22character%22%3A0%7D%5D "src/components/AwsFormComponents/enviromentAlias.tsx"), [`Radio`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FenviromentAlias.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FenviromentAlias.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FenviromentAlias.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A2%2C%22character%22%3A0%7D%5D "src/components/AwsFormComponents/enviromentAlias.tsx"), [`RadioGroup`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FenviromentAlias.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FenviromentAlias.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FenviromentAlias.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A2%2C%22character%22%3A0%7D%5D "src/components/AwsFormComponents/enviromentAlias.tsx"), [`TextField`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FenviromentAlias.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FenviromentAlias.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FenviromentAlias.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A2%2C%22character%22%3A0%7D%5D "src/components/AwsFormComponents/enviromentAlias.tsx")**: Components from Material-UI for form controls.
- **[`useState`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FenviromentAlias.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FenviromentAlias.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FenviromentAlias.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A10%2C%22character%22%3A0%7D%5D "src/components/AwsFormComponents/enviromentAlias.tsx")**: React hook for state management.
- **[`toast`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FenviromentAlias.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FenviromentAlias.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FenviromentAlias.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A11%2C%22character%22%3A0%7D%5D "src/components/AwsFormComponents/enviromentAlias.tsx")**: Library for displaying toast notifications.
- **[`SectionQuestion`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Futils%2Fconstants.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A123%2C%22character%22%3A0%7D%5D "src/utils/constants.tsx")**: Constant from a utility file.

#### Interface: [`selectRegionProps`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FenviromentAlias.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FenviromentAlias.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FenviromentAlias.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A14%2C%22character%22%3A0%7D%5D "src/components/AwsFormComponents/enviromentAlias.tsx")
Defines the properties expected by the [`EnviromentAlias`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FenviromentAlias.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FenviromentAlias.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FenviromentAlias.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A21%2C%22character%22%3A0%7D%5D "src/components/AwsFormComponents/enviromentAlias.tsx") component.
- **[`onNext`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FenviromentAlias.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FenviromentAlias.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FenviromentAlias.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A15%2C%22character%22%3A2%7D%5D "src/components/AwsFormComponents/enviromentAlias.tsx")**: Function to be called when moving to the next step.
- **[`onPrev`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FenviromentAlias.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FenviromentAlias.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FenviromentAlias.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A16%2C%22character%22%3A2%7D%5D "src/components/AwsFormComponents/enviromentAlias.tsx")**: Function to be called when moving to the previous step.
- **[`onData`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FenviromentAlias.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FenviromentAlias.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FenviromentAlias.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A17%2C%22character%22%3A2%7D%5D "src/components/AwsFormComponents/enviromentAlias.tsx")**: Function to be called with the alias data.

#### Component: [`EnviromentAlias`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FenviromentAlias.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FenviromentAlias.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FenviromentAlias.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A21%2C%22character%22%3A0%7D%5D "src/components/AwsFormComponents/enviromentAlias.tsx")
A React functional component that handles the environment alias input.

##### State
- **[`question`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FenviromentAlias.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FenviromentAlias.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FenviromentAlias.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A26%2C%22character%22%3A2%7D%5D "src/components/AwsFormComponents/enviromentAlias.tsx")**: Holds the question data from [`SectionQuestion`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Futils%2Fconstants.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A123%2C%22character%22%3A0%7D%5D "src/utils/constants.tsx").

##### Methods
- **[`handleOnClickNext`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FenviromentAlias.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FenviromentAlias.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FenviromentAlias.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A28%2C%22character%22%3A2%7D%5D "src/components/AwsFormComponents/enviromentAlias.tsx")**: Handles the click event for the "Next" button.
  - **Pseudo Logic**:
    1. Check if [`question.response`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FenviromentAlias.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FenviromentAlias.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FenviromentAlias.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A26%2C%22character%22%3A2%7D%5D "src/components/AwsFormComponents/enviromentAlias.tsx") is not null.
    2. If not null, call [`props.onData`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FenviromentAlias.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FenviromentAlias.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FenviromentAlias.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A21%2C%22character%22%3A54%7D%5D "src/components/AwsFormComponents/enviromentAlias.tsx") with [`question.response`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FenviromentAlias.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FenviromentAlias.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FenviromentAlias.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A26%2C%22character%22%3A2%7D%5D "src/components/AwsFormComponents/enviromentAlias.tsx") and then call [`props.onNext`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FenviromentAlias.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FenviromentAlias.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FenviromentAlias.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A21%2C%22character%22%3A54%7D%5D "src/components/AwsFormComponents/enviromentAlias.tsx").
    3. If null, display an error toast notification.

##### JSX Structure
- **Container**: A `div` with classes for styling.
- **Title**: A `div` displaying the text "Enter the Environment Name".

### Pseudo Logic
1. **Imports**: Import necessary libraries and components.
2. **Interface Definition**: Define the [`selectRegionProps`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FenviromentAlias.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FenviromentAlias.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FenviromentAlias.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A14%2C%22character%22%3A0%7D%5D "src/components/AwsFormComponents/enviromentAlias.tsx") interface.
3. **Component Definition**: Define the [`EnviromentAlias`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FenviromentAlias.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FenviromentAlias.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FenviromentAlias.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A21%2C%22character%22%3A0%7D%5D "src/components/AwsFormComponents/enviromentAlias.tsx") component.
   - Initialize the [`question`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FenviromentAlias.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FenviromentAlias.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FenviromentAlias.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A26%2C%22character%22%3A2%7D%5D "src/components/AwsFormComponents/enviromentAlias.tsx") state with data from [`SectionQuestion`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Futils%2Fconstants.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A123%2C%22character%22%3A0%7D%5D "src/utils/constants.tsx").
   - Define the [`handleOnClickNext`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FenviromentAlias.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FenviromentAlias.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FenviromentAlias.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A28%2C%22character%22%3A2%7D%5D "src/components/AwsFormComponents/enviromentAlias.tsx") method.
   - Return JSX structure with a container and title.

### Code
```tsx
import { faMagnifyingGlass } from "@fortawesome/free-solid-svg-icons";
import { FontAwesomeIcon } from "@fortawesome/react-fontawesome";
import {
  FormControl,
  FormControlLabel,
  InputAdornment,
  Radio,
  RadioGroup,
  TextField,
} from "@mui/material";
import { useState } from "react";
import toast from "react-hot-toast";
import { SectionQuestion } from "../../utils/constants";

interface selectRegionProps {
  onNext: () => void;
  onPrev: () => void;
  onData: (aliasData: string) => void;
  //currentComponent: number;
}

const EnviromentAlias: React.FC<selectRegionProps> = (props) => {
  // const [envAlias,setEnvAlias] = useState<any>(SectionQuestion[1]);
  // const question = envAlias;
  // setEnvAlias(question);

  const question = SectionQuestion[1];

  const handleOnClickNext = () => {
    if (question.response != null) {
      props.onData(question.response);
      props.onNext();
    } else {
      toast.error("Please Type Environment Alias First");
    }
  };

  return (
    <div className="bg-white mx-40 rounded-lg mt-2 h-[541px] shadow-lg relative mb-16 ">
      <div className="text-nblue-200 font-medium pt-10 px-12">
        Enter the Environment Name
      </div>
    </div>
  );
};

export default EnviromentAlias;
```

sumitchohan: generate detailed documentation for current code file including all methods, fields, properties and summary of their implementation including pseudo logic



GitHub Copilot: ### File: selectMethod.tsx

#### Summary
This file defines a React functional component named [`SelectMethod`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FselectMethod.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FselectMethod.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FselectMethod.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A26%2C%22character%22%3A0%7D%5D "src/components/AwsFormComponents/selectMethod.tsx") which is responsible for rendering a selection method interface. It uses various Material-UI components and FontAwesome icons to create a user interface. The component manages state for selected options and click events, and it interacts with an API to create an account.

#### Imports
- **FontAwesomeIcon**: Used to render FontAwesome icons.
- **Material-UI Components**: Includes [`FormControl`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FselectMethod.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FselectMethod.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FselectMethod.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A2%2C%22character%22%3A0%7D%5D "src/components/AwsFormComponents/selectMethod.tsx"), [`FormControlLabel`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FselectMethod.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FselectMethod.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FselectMethod.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A2%2C%22character%22%3A0%7D%5D "src/components/AwsFormComponents/selectMethod.tsx"), [`InputAdornment`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FselectMethod.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FselectMethod.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FselectMethod.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A2%2C%22character%22%3A0%7D%5D "src/components/AwsFormComponents/selectMethod.tsx"), [`Radio`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FselectMethod.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FselectMethod.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FselectMethod.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A2%2C%22character%22%3A0%7D%5D "src/components/AwsFormComponents/selectMethod.tsx"), [`RadioGroup`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FselectMethod.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FselectMethod.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FselectMethod.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A2%2C%22character%22%3A0%7D%5D "src/components/AwsFormComponents/selectMethod.tsx"), and [`TextField`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FselectMethod.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FselectMethod.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FselectMethod.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A2%2C%22character%22%3A0%7D%5D "src/components/AwsFormComponents/selectMethod.tsx") for building the form.
- **React Hooks**: [`useEffect`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FselectMethod.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FselectMethod.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FselectMethod.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A10%2C%22character%22%3A0%7D%5D "src/components/AwsFormComponents/selectMethod.tsx") and [`useState`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FselectMethod.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FselectMethod.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FselectMethod.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A10%2C%22character%22%3A0%7D%5D "src/components/AwsFormComponents/selectMethod.tsx") for managing component state and side effects.
- **IconConsole**: An SVG icon imported from local assets.
- **createAccountAPI**: An API function to create an account.
- **toast**: A library for displaying toast notifications.
- **SectionQuestion**: A constant imported from utilities.

#### Interface: [`selectRegionProps`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FselectMethod.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FselectMethod.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FselectMethod.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A16%2C%22character%22%3A0%7D%5D "src/components/AwsFormComponents/selectMethod.tsx")
Defines the properties expected by the [`SelectMethod`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FselectMethod.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FselectMethod.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FselectMethod.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A26%2C%22character%22%3A0%7D%5D "src/components/AwsFormComponents/selectMethod.tsx") component:
- **onNext**: Function to be called when moving to the next step.
- **onPrev**: Function to be called when moving to the previous step.
- **regionData**: Array of strings representing region data.
- **aliasData**: String representing alias data.
- **onData**: Function to handle data with parameters [`uniqueIdVal`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FselectMethod.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FselectMethod.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FselectMethod.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A21%2C%22character%22%3A11%7D%5D "src/components/AwsFormComponents/selectMethod.tsx") and [`externalIdVal`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FselectMethod.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FselectMethod.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FselectMethod.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A21%2C%22character%22%3A32%7D%5D "src/components/AwsFormComponents/selectMethod.tsx").
- **selectionData**: Function to handle the selected option with a parameter [`selectedOption`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FselectMethod.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FselectMethod.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FselectMethod.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A22%2C%22character%22%3A18%7D%5D "src/components/AwsFormComponents/selectMethod.tsx").

#### Component: [`SelectMethod`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FselectMethod.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FselectMethod.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FselectMethod.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A26%2C%22character%22%3A0%7D%5D "src/components/AwsFormComponents/selectMethod.tsx")
A React functional component that takes [`selectRegionProps`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FselectMethod.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FselectMethod.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FselectMethod.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A16%2C%22character%22%3A0%7D%5D "src/components/AwsFormComponents/selectMethod.tsx") as props.

##### State Variables
- **selectedOption**: State to track the selected option (number or null).
- **isClicked**: State to track if an option has been clicked (boolean or null).

##### Local Variables
- **uniqueId**: Initially set to `null`, intended to store a unique identifier.
- **externalId**: Initially set to `null`, intended to store an external identifier.
- **question**: Retrieves a specific question from [`SectionQuestion`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Futils%2Fconstants.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A123%2C%22character%22%3A0%7D%5D "src/utils/constants.tsx").

##### Console Log
- Logs the current value of [`selectedOption`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FselectMethod.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FselectMethod.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FselectMethod.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A22%2C%22character%22%3A18%7D%5D "src/components/AwsFormComponents/selectMethod.tsx") to the console.

##### Commented Code
- **uniqueIdAPI**: A commented-out function intended to call [`createAccountAPI`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FcreateAccount.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A22%2C%22character%22%3A0%7D%5D "src/apis/createAccount.tsx") with parameters "AWS", [`props.regionData`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FselectMethod.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FselectMethod.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FselectMethod.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A26%2C%22character%22%3A51%7D%5D "src/components/AwsFormComponents/selectMethod.tsx"), and [`props.aliasData`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FselectMethod.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FselectMethod.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FselectMethod.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A26%2C%22character%22%3A51%7D%5D "src/components/AwsFormComponents/selectMethod.tsx"), and store the response in `unique_id`.

### Pseudo Logic
1. **Imports**: Import necessary libraries and components.
2. **Interface Definition**: Define [`selectRegionProps`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FselectMethod.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FselectMethod.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FselectMethod.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A16%2C%22character%22%3A0%7D%5D "src/components/AwsFormComponents/selectMethod.tsx") to specify the expected props.
3. **Component Definition**: Define the [`SelectMethod`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FselectMethod.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FselectMethod.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FselectMethod.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A26%2C%22character%22%3A0%7D%5D "src/components/AwsFormComponents/selectMethod.tsx") component.
   - **State Initialization**: Initialize [`selectedOption`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FselectMethod.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FselectMethod.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FselectMethod.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A22%2C%22character%22%3A18%7D%5D "src/components/AwsFormComponents/selectMethod.tsx") and [`isClicked`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FselectMethod.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FselectMethod.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FselectMethod.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A28%2C%22character%22%3A2%7D%5D "src/components/AwsFormComponents/selectMethod.tsx") state variables.
   - **Local Variables**: Define [`uniqueId`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FselectMethod.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FselectMethod.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FselectMethod.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A29%2C%22character%22%3A2%7D%5D "src/components/AwsFormComponents/selectMethod.tsx"), [`externalId`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FselectMethod.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FselectMethod.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FselectMethod.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A30%2C%22character%22%3A2%7D%5D "src/components/AwsFormComponents/selectMethod.tsx"), and [`question`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FselectMethod.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FselectMethod.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FselectMethod.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A32%2C%22character%22%3A2%7D%5D "src/components/AwsFormComponents/selectMethod.tsx").
   - **Console Logging**: Log the [`selectedOption`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FselectMethod.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FselectMethod.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FselectMethod.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A22%2C%22character%22%3A18%7D%5D "src/components/AwsFormComponents/selectMethod.tsx") state to the console.
   - **Commented Function**: Define a commented-out function `uniqueIdAPI` to interact with the [`createAccountAPI`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FcreateAccount.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A22%2C%22character%22%3A0%7D%5D "src/apis/createAccount.tsx").

### Code
```tsx
import { faMagnifyingGlass } from "@fortawesome/free-solid-svg-icons";
import { FontAwesomeIcon } from "@fortawesome/react-fontawesome";
import {
  FormControl,
  FormControlLabel,
  InputAdornment,
  Radio,
  RadioGroup,
  TextField,
} from "@mui/material";
import { useEffect, useState } from "react";
import IconConsole from "../../assets/icons/icon-console.svg";
import { createAccountAPI } from "../../apis/createAccount";
import toast from "react-hot-toast";
import { SectionQuestion } from "../../utils/constants";

interface selectRegionProps {
  onNext: () => void;
  onPrev: () => void;
  regionData: string[];
  aliasData: string;
  onData: (uniqueIdVal: string, externalIdVal: string) => void;
  selectionData: (selectedOption: number) => void;
}

const SelectMethod: React.FC<selectRegionProps> = (props) => {
  const [selectedOption, setSelectedOption] = useState<number | null>(0);
  const [isClicked, setIsClicked] = useState<boolean | null>(null);
  let uniqueId = null;
  let externalId = null;

  const question = SectionQuestion[2];

  console.log(selectedOption);

  // const uniqueIdAPI = () => {
  //   const response = createAccountAPI("AWS", props.regionData, props.aliasData);
  //   const unique_id = response;
  // }
};
```

sumitchohan: generate detailed documentation for current code file including all methods, fields, properties and summary of their implementation including pseudo logic

GitHub Copilot: ### Documentation for [`selectRegion.tsx`](command:_github.copilot.openRelativePath?%5B%7B%22scheme%22%3A%22file%22%2C%22authority%22%3A%22%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FselectRegion.tsx%22%2C%22query%22%3A%22%22%2C%22fragment%22%3A%22%22%7D%5D "/Users/sumit/Downloads/Frontend/NAGISVPR/src/components/AwsFormComponents/selectRegion.tsx")

#### Summary
This file defines a React functional component named [`SelectRegion`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FselectRegion.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FselectRegion.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FselectRegion.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A28%2C%22character%22%3A0%7D%5D "src/components/AwsFormComponents/selectRegion.tsx"). The component is responsible for rendering a UI that allows users to select a region. It uses various UI components from the Material-UI library and FontAwesome icons. The component also manages state and handles user interactions such as searching and selecting regions.

#### Imports
- **FontAwesome Icons**: [`faMagnifyingGlass`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FselectRegion.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FselectRegion.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FselectRegion.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A0%2C%22character%22%3A0%7D%5D "src/components/AwsFormComponents/selectRegion.tsx"), [`faWindowRestore`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FselectRegion.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FselectRegion.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FselectRegion.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A0%2C%22character%22%3A0%7D%5D "src/components/AwsFormComponents/selectRegion.tsx")
- **FontAwesome Component**: [`FontAwesomeIcon`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FselectRegion.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FselectRegion.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FselectRegion.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A4%2C%22character%22%3A0%7D%5D "src/components/AwsFormComponents/selectRegion.tsx")
- **Material-UI Components**: [`Checkbox`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FselectRegion.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FselectRegion.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FselectRegion.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A5%2C%22character%22%3A0%7D%5D "src/components/AwsFormComponents/selectRegion.tsx"), [`FormControl`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FselectRegion.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FselectRegion.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FselectRegion.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A5%2C%22character%22%3A0%7D%5D "src/components/AwsFormComponents/selectRegion.tsx"), [`FormControlLabel`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FselectRegion.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FselectRegion.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FselectRegion.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A5%2C%22character%22%3A0%7D%5D "src/components/AwsFormComponents/selectRegion.tsx"), [`FormGroup`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FselectRegion.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FselectRegion.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FselectRegion.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A5%2C%22character%22%3A0%7D%5D "src/components/AwsFormComponents/selectRegion.tsx"), [`InputAdornment`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FselectRegion.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FselectRegion.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FselectRegion.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A5%2C%22character%22%3A0%7D%5D "src/components/AwsFormComponents/selectRegion.tsx"), [`Radio`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FselectRegion.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FselectRegion.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FselectRegion.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A5%2C%22character%22%3A0%7D%5D "src/components/AwsFormComponents/selectRegion.tsx"), [`RadioGroup`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FselectRegion.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FselectRegion.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FselectRegion.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A5%2C%22character%22%3A0%7D%5D "src/components/AwsFormComponents/selectRegion.tsx"), [`TextField`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FselectRegion.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FselectRegion.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FselectRegion.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A5%2C%22character%22%3A0%7D%5D "src/components/AwsFormComponents/selectRegion.tsx")
- **React Hooks**: [`useEffect`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FselectRegion.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FselectRegion.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FselectRegion.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A15%2C%22character%22%3A0%7D%5D "src/components/AwsFormComponents/selectRegion.tsx"), [`useState`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FselectRegion.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FselectRegion.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FselectRegion.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A15%2C%22character%22%3A0%7D%5D "src/components/AwsFormComponents/selectRegion.tsx")
- **Custom Component**: [`SelectCloud`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FselectCloud.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A17%2C%22character%22%3A0%7D%5D "src/components/selectCloud.tsx")
- **React Router Hook**: [`useNavigate`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FselectRegion.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FselectRegion.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FselectRegion.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A17%2C%22character%22%3A0%7D%5D "src/components/AwsFormComponents/selectRegion.tsx")
- **Toast Notification**: [`toast`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FselectRegion.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FselectRegion.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FselectRegion.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A18%2C%22character%22%3A0%7D%5D "src/components/AwsFormComponents/selectRegion.tsx")
- **Constants**: [`SectionQuestion`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Futils%2Fconstants.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A123%2C%22character%22%3A0%7D%5D "src/utils/constants.tsx") from `../../utils/constants`

#### Interface
```tsx
interface selectRegionProps {
  onNext: () => void;
  onPrev: () => void;
  onData: (regionData: string[]) => void;
  formData: { alias: string; region: string[] };
}
```
- **onNext**: Callback function to proceed to the next step.
- **onPrev**: Callback function to go back to the previous step.
- **onData**: Callback function to handle the selected region data.
- **formData**: Object containing `alias` (string) and `region` (array of strings).

#### Component: `SelectRegion`
```tsx
const SelectRegion: React.FC<selectRegionProps> = (props) => {
  // State variables
  const [searchQuery, setSearchQuery] = useState<string>("");
  const [currentQuestion, setCurrentQuestion] = useState<number>(0);
  const [selectedOption, setSelectedOption] = useState<string[]>(props.formData.region);
  const [selectedOptions, setSelectedOptions] = useState<string[]>([]);
  const [buttonClicked, setButtonClicked] = useState<boolean>(false);

  // Effect to update selected options when formData.region changes
  useEffect(() => {
    setSelectedOptions(props.formData.region);
  }, [props.formData.region]);
```

#### State Variables
- **searchQuery**: Stores the current search query entered by the user.
- **currentQuestion**: Tracks the current question index.
- **selectedOption**: Stores the currently selected region options.
- **selectedOptions**: Stores the selected options, initialized with [`props.formData.region`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FselectRegion.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FselectRegion.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FselectRegion.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A28%2C%22character%22%3A51%7D%5D "src/components/AwsFormComponents/selectRegion.tsx").
- **buttonClicked**: Boolean flag to track if a button has been clicked.

#### useEffect Hook
- **Purpose**: Updates the [`selectedOptions`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FselectRegion.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FselectRegion.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FselectRegion.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A34%2C%22character%22%3A2%7D%5D "src/components/AwsFormComponents/selectRegion.tsx") state whenever [`props.formData.region`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FselectRegion.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FselectRegion.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FselectRegion.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A28%2C%22character%22%3A51%7D%5D "src/components/AwsFormComponents/selectRegion.tsx") changes.
- **Dependencies**: [`props.formData.region`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FselectRegion.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FselectRegion.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FselectRegion.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A28%2C%22character%22%3A51%7D%5D "src/components/AwsFormComponents/selectRegion.tsx")

#### Pseudo Logic
1. **Initialize State**: Define state variables for managing search query, current question, selected options, and button click status.
2. **Effect Hook**: Update [`selectedOptions`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FselectRegion.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FselectRegion.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FselectRegion.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A34%2C%22character%22%3A2%7D%5D "src/components/AwsFormComponents/selectRegion.tsx") whenever [`props.formData.region`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FselectRegion.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FselectRegion.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FselectRegion.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A28%2C%22character%22%3A51%7D%5D "src/components/AwsFormComponents/selectRegion.tsx") changes.
3. **Render UI**: (Not shown in the provided code) Render the UI components including text fields, checkboxes, radio buttons, and other form controls.
4. **Handle User Interactions**: (Not shown in the provided code) Implement functions to handle user interactions such as searching, selecting options, and navigating between steps.
5. **Callbacks**: Use the provided callback functions ([`onNext`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FselectRegion.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FselectRegion.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FselectRegion.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A22%2C%22character%22%3A2%7D%5D "src/components/AwsFormComponents/selectRegion.tsx"), [`onPrev`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FselectRegion.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FselectRegion.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FselectRegion.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A23%2C%22character%22%3A2%7D%5D "src/components/AwsFormComponents/selectRegion.tsx"), [`onData`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FselectRegion.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FselectRegion.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FselectRegion.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A24%2C%22character%22%3A2%7D%5D "src/components/AwsFormComponents/selectRegion.tsx")) to communicate with parent components.

This documentation covers the initial part of the [`SelectRegion`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FselectRegion.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FselectRegion.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAwsFormComponents%2FselectRegion.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A28%2C%22character%22%3A0%7D%5D "src/components/AwsFormComponents/selectRegion.tsx") component. The full implementation would include rendering the UI and handling various user interactions, which are not shown in the provided code excerpt.

sumitchohan: generate detailed documentation for current code file including all methods, fields, properties and summary of their implementation including pseudo logic

GitHub Copilot: ### AzureAccountAlias.tsx Documentation

#### Summary
This file defines a React functional component named [`AzureAccountAlias`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAzureFormComponents%2FAzureAccountAlias.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAzureFormComponents%2FAzureAccountAlias.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAzureFormComponents%2FAzureAccountAlias.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A23%2C%22character%22%3A0%7D%5D "src/components/AzureFormComponents/AzureAccountAlias.tsx"). The component is responsible for managing and validating an alias for an Azure account. It includes form controls for user input and fetches existing aliases from an API.

#### Imports
- **FontAwesomeIcon**: Used for rendering icons.
- **Material UI Components**: Various form controls from Material UI.
- **React Hooks**: [`useEffect`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAzureFormComponents%2FAzureAccountAlias.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAzureFormComponents%2FAzureAccountAlias.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAzureFormComponents%2FAzureAccountAlias.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A10%2C%22character%22%3A0%7D%5D "src/components/AzureFormComponents/AzureAccountAlias.tsx") and [`useState`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAzureFormComponents%2FAzureAccountAlias.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAzureFormComponents%2FAzureAccountAlias.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAzureFormComponents%2FAzureAccountAlias.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A10%2C%22character%22%3A0%7D%5D "src/components/AzureFormComponents/AzureAccountAlias.tsx") for managing component state and side effects.
- **toast**: For displaying notifications.
- **getAllAccounts**: API function to fetch all accounts.

#### Interfaces
- **accountAliasProps**
  - [`onNext`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAzureFormComponents%2FAzureAccountAlias.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAzureFormComponents%2FAzureAccountAlias.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAzureFormComponents%2FAzureAccountAlias.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A15%2C%22character%22%3A2%7D%5D "src/components/AzureFormComponents/AzureAccountAlias.tsx"): Function to be called when moving to the next step.
  - [`onPrev`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAzureFormComponents%2FAzureAccountAlias.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAzureFormComponents%2FAzureAccountAlias.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAzureFormComponents%2FAzureAccountAlias.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A16%2C%22character%22%3A2%7D%5D "src/components/AzureFormComponents/AzureAccountAlias.tsx"): Function to be called when moving to the previous step.
  - [`onData`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAzureFormComponents%2FAzureAccountAlias.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAzureFormComponents%2FAzureAccountAlias.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAzureFormComponents%2FAzureAccountAlias.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A17%2C%22character%22%3A2%7D%5D "src/components/AzureFormComponents/AzureAccountAlias.tsx"): Function to handle the alias data.
  - [`initialAlias`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAzureFormComponents%2FAzureAccountAlias.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAzureFormComponents%2FAzureAccountAlias.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAzureFormComponents%2FAzureAccountAlias.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A18%2C%22character%22%3A2%7D%5D "src/components/AzureFormComponents/AzureAccountAlias.tsx") (optional): Initial alias value.
  - [`formData`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAzureFormComponents%2FAzureAccountAlias.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAzureFormComponents%2FAzureAccountAlias.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAzureFormComponents%2FAzureAccountAlias.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A19%2C%22character%22%3A2%7D%5D "src/components/AzureFormComponents/AzureAccountAlias.tsx"): Object containing form data with an alias field.

#### Component: AzureAccountAlias
- **Props**
  - [`onNext`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAzureFormComponents%2FAzureAccountAlias.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAzureFormComponents%2FAzureAccountAlias.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAzureFormComponents%2FAzureAccountAlias.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A15%2C%22character%22%3A2%7D%5D "src/components/AzureFormComponents/AzureAccountAlias.tsx"): Function to proceed to the next step.
  - [`onPrev`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAzureFormComponents%2FAzureAccountAlias.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAzureFormComponents%2FAzureAccountAlias.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAzureFormComponents%2FAzureAccountAlias.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A16%2C%22character%22%3A2%7D%5D "src/components/AzureFormComponents/AzureAccountAlias.tsx"): Function to go back to the previous step.
  - [`onData`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAzureFormComponents%2FAzureAccountAlias.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAzureFormComponents%2FAzureAccountAlias.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAzureFormComponents%2FAzureAccountAlias.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A17%2C%22character%22%3A2%7D%5D "src/components/AzureFormComponents/AzureAccountAlias.tsx"): Function to handle alias data.
  - [`initialAlias`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAzureFormComponents%2FAzureAccountAlias.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAzureFormComponents%2FAzureAccountAlias.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAzureFormComponents%2FAzureAccountAlias.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A18%2C%22character%22%3A2%7D%5D "src/components/AzureFormComponents/AzureAccountAlias.tsx"): Initial alias value, defaults to an empty string.
  - [`formData`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAzureFormComponents%2FAzureAccountAlias.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAzureFormComponents%2FAzureAccountAlias.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAzureFormComponents%2FAzureAccountAlias.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A19%2C%22character%22%3A2%7D%5D "src/components/AzureFormComponents/AzureAccountAlias.tsx"): Object containing form data with an alias field.

- **State Variables**
  - [`accountAlias`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAzureFormComponents%2FAzureAccountAlias.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAzureFormComponents%2FAzureAccountAlias.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAzureFormComponents%2FAzureAccountAlias.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A30%2C%22character%22%3A2%7D%5D "src/components/AzureFormComponents/AzureAccountAlias.tsx"): Stores the current alias value.
  - [`isValid`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAzureFormComponents%2FAzureAccountAlias.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAzureFormComponents%2FAzureAccountAlias.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAzureFormComponents%2FAzureAccountAlias.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A33%2C%22character%22%3A2%7D%5D "src/components/AzureFormComponents/AzureAccountAlias.tsx"): Boolean indicating if the alias is valid.
  - [`existingAliases`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAzureFormComponents%2FAzureAccountAlias.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAzureFormComponents%2FAzureAccountAlias.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAzureFormComponents%2FAzureAccountAlias.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A35%2C%22character%22%3A2%7D%5D "src/components/AzureFormComponents/AzureAccountAlias.tsx"): Stores the list of existing aliases fetched from the API.

- **useEffect Hook**
  - **Purpose**: Fetch existing aliases when the component mounts.
  - **Pseudo Logic**:
    1. Define an asynchronous function [`fetchData`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAzureFormComponents%2FAzureAccountAlias.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAzureFormComponents%2FAzureAccountAlias.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAzureFormComponents%2FAzureAccountAlias.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A38%2C%22character%22%3A4%7D%5D "src/components/AzureFormComponents/AzureAccountAlias.tsx").
    2. Inside [`fetchData`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAzureFormComponents%2FAzureAccountAlias.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAzureFormComponents%2FAzureAccountAlias.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAzureFormComponents%2FAzureAccountAlias.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A38%2C%22character%22%3A4%7D%5D "src/components/AzureFormComponents/AzureAccountAlias.tsx"), call [`getAllAccounts`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FaccountAPI.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A51%2C%22character%22%3A0%7D%5D "src/apis/accountAPI.tsx") to fetch aliases.
    3. Handle the result and update [`existingAliases`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAzureFormComponents%2FAzureAccountAlias.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAzureFormComponents%2FAzureAccountAlias.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAzureFormComponents%2FAzureAccountAlias.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A35%2C%22character%22%3A2%7D%5D "src/components/AzureFormComponents/AzureAccountAlias.tsx") state.
    4. Handle any errors by displaying a toast notification.

#### Pseudo Logic for Component
1. Initialize state variables for [`accountAlias`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAzureFormComponents%2FAzureAccountAlias.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAzureFormComponents%2FAzureAccountAlias.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAzureFormComponents%2FAzureAccountAlias.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A30%2C%22character%22%3A2%7D%5D "src/components/AzureFormComponents/AzureAccountAlias.tsx"), [`isValid`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAzureFormComponents%2FAzureAccountAlias.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAzureFormComponents%2FAzureAccountAlias.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAzureFormComponents%2FAzureAccountAlias.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A33%2C%22character%22%3A2%7D%5D "src/components/AzureFormComponents/AzureAccountAlias.tsx"), and [`existingAliases`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAzureFormComponents%2FAzureAccountAlias.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAzureFormComponents%2FAzureAccountAlias.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAzureFormComponents%2FAzureAccountAlias.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A35%2C%22character%22%3A2%7D%5D "src/components/AzureFormComponents/AzureAccountAlias.tsx").
2. Use [`useEffect`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAzureFormComponents%2FAzureAccountAlias.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAzureFormComponents%2FAzureAccountAlias.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAzureFormComponents%2FAzureAccountAlias.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A10%2C%22character%22%3A0%7D%5D "src/components/AzureFormComponents/AzureAccountAlias.tsx") to fetch existing aliases when the component mounts.
3. Render form controls for alias input.
4. Validate the alias input and update the state accordingly.
5. Provide navigation functions ([`onNext`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAzureFormComponents%2FAzureAccountAlias.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAzureFormComponents%2FAzureAccountAlias.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAzureFormComponents%2FAzureAccountAlias.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A15%2C%22character%22%3A2%7D%5D "src/components/AzureFormComponents/AzureAccountAlias.tsx") and [`onPrev`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAzureFormComponents%2FAzureAccountAlias.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAzureFormComponents%2FAzureAccountAlias.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAzureFormComponents%2FAzureAccountAlias.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A16%2C%22character%22%3A2%7D%5D "src/components/AzureFormComponents/AzureAccountAlias.tsx")) to move between steps.
6. Use [`onData`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAzureFormComponents%2FAzureAccountAlias.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAzureFormComponents%2FAzureAccountAlias.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAzureFormComponents%2FAzureAccountAlias.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A17%2C%22character%22%3A2%7D%5D "src/components/AzureFormComponents/AzureAccountAlias.tsx") to pass the alias data to the parent component.

#### Code
```tsx
import { faMagnifyingGlass } from "@fortawesome/free-solid-svg-icons";
import { FontAwesomeIcon } from "@fortawesome/react-fontawesome";
import {
  FormControl,
  FormControlLabel,
  InputAdornment,
  Radio,
  RadioGroup,
  TextField,
} from "@mui/material";
import { useEffect, useState } from "react";
import toast from "react-hot-toast";
import { getAllAccounts } from "../../apis/accountAPI";

interface accountAliasProps {
  onNext: () => void;
  onPrev: () => void;
  onData: (aliasData: string) => void;
  initialAlias?: string;
  formData: { alias: string };
}

const AzureAccountAlias: React.FC<accountAliasProps> = ({
  onNext,
  onPrev,
  onData,
  initialAlias = "",
  formData,
}) => {
  const [accountAlias, setAccountAlias] = useState<string>(
    formData.alias || ""
  );
  const [isValid, setIsValid] = useState<boolean>(true);
  const [existingAliases, setExistingAliases] = useState<any>();

  useEffect(() => {
    const fetchData = async () => {
      try {
        // const result = await getAllAccounts("UniqueId", "CloudProvider");
        // setExistingAliases(result);
      } catch (error) {
        toast.error("Failed to fetch account aliases");
      }
    };

    fetchData();
  }, []);

  // Additional component logic and JSX rendering here

  return (
    <div>
      {/* JSX for form controls and other UI elements */}
    </div>
  );
};

export default AzureAccountAlias;
```

This documentation provides a detailed overview of the `AzureAccountAlias` component, including its purpose, props, state variables, and the logic implemented within the component.

sumitchohan: generate detailed documentation for current code file including all methods, fields, properties and summary of their implementation including pseudo logic


GitHub Copilot: ### AzureCli.tsx Documentation

#### Summary
This file defines a React functional component named [`AzureCli`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAzureFormComponents%2FAzureCli.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAzureFormComponents%2FAzureCli.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAzureFormComponents%2FAzureCli.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A21%2C%22character%22%3A0%7D%5D "src/components/AzureFormComponents/AzureCli.tsx"). The component is designed to interact with Azure CLI templates and includes functionalities such as copying text to the clipboard, managing state for various UI elements, and fetching configuration data from an API.

#### Imports
- **`@fortawesome/free-regular-svg-icons/faAddressBook`**: Imports a unicode icon from FontAwesome.
- **`react`**: Imports React and its hooks ([`useEffect`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAzureFormComponents%2FAzureCli.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAzureFormComponents%2FAzureCli.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAzureFormComponents%2FAzureCli.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A1%2C%22character%22%3A0%7D%5D "src/components/AzureFormComponents/AzureCli.tsx"), [`useState`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAzureFormComponents%2FAzureCli.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAzureFormComponents%2FAzureCli.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAzureFormComponents%2FAzureCli.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A1%2C%22character%22%3A0%7D%5D "src/components/AzureFormComponents/AzureCli.tsx")).
- **`react-copy-to-clipboard`**: Imports a component to facilitate copying text to the clipboard.
- **`../../apis/createAccountCheckStack`**: Imports a function to check account stack creation.
- **`@mui/material/styles/createTransitions`**: Imports a function to create transitions (not used in the current code).
- **`react-router-dom`**: Imports [`useNavigate`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAzureFormComponents%2FAzureCli.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAzureFormComponents%2FAzureCli.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAzureFormComponents%2FAzureCli.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A5%2C%22character%22%3A0%7D%5D "src/components/AzureFormComponents/AzureCli.tsx") for navigation (not used in the current code).
- **`../../services/authService`**: Imports a function to get registered user information (not used in the current code).
- **`../../utils/interfaces`**: Imports the [`User`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Futils%2Finterfaces.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A50%2C%22character%22%3A0%7D%5D "src/utils/interfaces.tsx") interface (not used in the current code).
- **`../../apis/loginAPI`**: Imports a function to get configuration data.
- **`../../apis/azureAccountCreation`**: Imports a function for Azure account creation (not used in the current code).
- **`@mui/material/TextField`**: Imports a Material-UI TextField component.

#### Interfaces
- **[`AzureCliProps`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAzureFormComponents%2FAzureCli.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAzureFormComponents%2FAzureCli.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAzureFormComponents%2FAzureCli.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A12%2C%22character%22%3A0%7D%5D "src/components/AzureFormComponents/AzureCli.tsx")**: Defines the properties that the [`AzureCli`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAzureFormComponents%2FAzureCli.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAzureFormComponents%2FAzureCli.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAzureFormComponents%2FAzureCli.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A21%2C%22character%22%3A0%7D%5D "src/components/AzureFormComponents/AzureCli.tsx") component expects.
  - [`aliasData`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAzureFormComponents%2FAzureCli.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAzureFormComponents%2FAzureCli.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAzureFormComponents%2FAzureCli.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A13%2C%22character%22%3A2%7D%5D "src/components/AzureFormComponents/AzureCli.tsx"): A string representing alias data.
  - [`onCopy?`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAzureFormComponents%2FAzureCli.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAzureFormComponents%2FAzureCli.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAzureFormComponents%2FAzureCli.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A14%2C%22character%22%3A2%7D%5D "src/components/AzureFormComponents/AzureCli.tsx"): An optional callback function to be called when text is copied.
  - [`uniqueId?`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAzureFormComponents%2FAzureCli.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAzureFormComponents%2FAzureCli.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAzureFormComponents%2FAzureCli.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A15%2C%22character%22%3A2%7D%5D "src/components/AzureFormComponents/AzureCli.tsx"): An optional unique identifier.
  - [`externalId?`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAzureFormComponents%2FAzureCli.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAzureFormComponents%2FAzureCli.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAzureFormComponents%2FAzureCli.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A16%2C%22character%22%3A2%7D%5D "src/components/AzureFormComponents/AzureCli.tsx"): An optional external identifier.
  - [`onPrev?`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAzureFormComponents%2FAzureCli.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAzureFormComponents%2FAzureCli.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAzureFormComponents%2FAzureCli.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A17%2C%22character%22%3A2%7D%5D "src/components/AzureFormComponents/AzureCli.tsx"): An optional callback function for a previous action.
  - [`switchData`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAzureFormComponents%2FAzureCli.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAzureFormComponents%2FAzureCli.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAzureFormComponents%2FAzureCli.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A18%2C%22character%22%3A2%7D%5D "src/components/AzureFormComponents/AzureCli.tsx"): A function to switch data, taking a boolean and a number as arguments.

#### Component: [`AzureCli`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAzureFormComponents%2FAzureCli.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAzureFormComponents%2FAzureCli.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAzureFormComponents%2FAzureCli.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A21%2C%22character%22%3A0%7D%5D "src/components/AzureFormComponents/AzureCli.tsx")
- **Props**: Accepts [`AzureCliProps`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAzureFormComponents%2FAzureCli.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAzureFormComponents%2FAzureCli.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAzureFormComponents%2FAzureCli.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A12%2C%22character%22%3A0%7D%5D "src/components/AzureFormComponents/AzureCli.tsx").
- **State Variables**:
  - [`copied`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAzureFormComponents%2FAzureCli.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAzureFormComponents%2FAzureCli.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAzureFormComponents%2FAzureCli.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A22%2C%22character%22%3A2%7D%5D "src/components/AzureFormComponents/AzureCli.tsx"): Boolean state to track if text has been copied.
  - [`stackEnable`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAzureFormComponents%2FAzureCli.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAzureFormComponents%2FAzureCli.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAzureFormComponents%2FAzureCli.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A23%2C%22character%22%3A2%7D%5D "src/components/AzureFormComponents/AzureCli.tsx"): Boolean state to track if the stack is enabled.
  - [`templateUrl`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAzureFormComponents%2FAzureCli.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAzureFormComponents%2FAzureCli.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAzureFormComponents%2FAzureCli.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A24%2C%22character%22%3A2%7D%5D "src/components/AzureFormComponents/AzureCli.tsx"): String state to store the template URL.
  - [`hasStartedTyping`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAzureFormComponents%2FAzureCli.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAzureFormComponents%2FAzureCli.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAzureFormComponents%2FAzureCli.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A25%2C%22character%22%3A2%7D%5D "src/components/AzureFormComponents/AzureCli.tsx"): Boolean state to track if typing has started.
  - [`copiedText`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAzureFormComponents%2FAzureCli.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAzureFormComponents%2FAzureCli.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAzureFormComponents%2FAzureCli.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A26%2C%22character%22%3A2%7D%5D "src/components/AzureFormComponents/AzureCli.tsx"): String state to store the copied text.
  - [`applyDisable`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAzureFormComponents%2FAzureCli.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAzureFormComponents%2FAzureCli.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAzureFormComponents%2FAzureCli.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A27%2C%22character%22%3A2%7D%5D "src/components/AzureFormComponents/AzureCli.tsx"): Boolean state to track if the apply button is disabled.

#### useEffect Hook
- **Purpose**: Fetches configuration data when the component mounts.
- **Pseudo Logic**:
  1. Define an asynchronous function.
  2. Call [`getConfiguration`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FloginAPI.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A3%2C%22character%22%3A0%7D%5D "src/apis/loginAPI.tsx") to fetch configuration data.
  3. Set [`templateUrl`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAzureFormComponents%2FAzureCli.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAzureFormComponents%2FAzureCli.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAzureFormComponents%2FAzureCli.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A24%2C%22character%22%3A2%7D%5D "src/components/AzureFormComponents/AzureCli.tsx") state with the fetched `azure_cli_template_url`.

#### Methods
- **[`handleCopy`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAzureFormComponents%2FAzureCli.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAzureFormComponents%2FAzureCli.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAzureFormComponents%2FAzureCli.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A37%2C%22character%22%3A2%7D%5D "src/components/AzureFormComponents/AzureCli.tsx")**: Handles the copy action.
  - **Pseudo Logic**:
    1. Set [`copied`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAzureFormComponents%2FAzureCli.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAzureFormComponents%2FAzureCli.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAzureFormComponents%2FAzureCli.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A22%2C%22character%22%3A2%7D%5D "src/components/AzureFormComponents/AzureCli.tsx") state to `true`.
    2. Call the optional [`onCopy`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAzureFormComponents%2FAzureCli.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAzureFormComponents%2FAzureCli.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAzureFormComponents%2FAzureCli.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A14%2C%22character%22%3A2%7D%5D "src/components/AzureFormComponents/AzureCli.tsx") callback if provided.

#### Example Usage
```tsx
<AzureCli
  aliasData="exampleAlias"
  onCopy={() => console.log("Copied!")}
  uniqueId="12345"
  externalId="67890"
  onPrev={() => console.log("Previous action")}
  switchData={(boolData, switchNumber) => console.log(boolData, switchNumber)}
/>
```

This documentation provides a detailed overview of the `AzureCli` component, including its imports, interfaces, state variables, and methods. The pseudo logic helps to understand the flow of data and actions within the component.

sumitchohan: generate detailed documentation for current code file including all methods, fields, properties and summary of their implementation including pseudo logic


GitHub Copilot: ### AzureSelectMethod.tsx Documentation

#### Summary
This file defines a React functional component [`AzureSelectMethod`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAzureFormComponents%2FAzureSelectMethod.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAzureFormComponents%2FAzureSelectMethod.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAzureFormComponents%2FAzureSelectMethod.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A26%2C%22character%22%3A0%7D%5D "src/components/AzureFormComponents/AzureSelectMethod.tsx") which is used to select a region and handle user interactions related to region selection. It uses various UI components from the Material-UI library and FontAwesome for icons. The component also interacts with an API to create an account and uses the `react-hot-toast` library for notifications.

#### Imports
- **FontAwesomeIcon**: Used for displaying icons.
- **Material-UI Components**: [`FormControl`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAzureFormComponents%2FAzureSelectMethod.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAzureFormComponents%2FAzureSelectMethod.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAzureFormComponents%2FAzureSelectMethod.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A2%2C%22character%22%3A0%7D%5D "src/components/AzureFormComponents/AzureSelectMethod.tsx"), [`FormControlLabel`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAzureFormComponents%2FAzureSelectMethod.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAzureFormComponents%2FAzureSelectMethod.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAzureFormComponents%2FAzureSelectMethod.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A2%2C%22character%22%3A0%7D%5D "src/components/AzureFormComponents/AzureSelectMethod.tsx"), [`InputAdornment`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAzureFormComponents%2FAzureSelectMethod.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAzureFormComponents%2FAzureSelectMethod.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAzureFormComponents%2FAzureSelectMethod.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A2%2C%22character%22%3A0%7D%5D "src/components/AzureFormComponents/AzureSelectMethod.tsx"), [`Radio`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAzureFormComponents%2FAzureSelectMethod.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAzureFormComponents%2FAzureSelectMethod.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAzureFormComponents%2FAzureSelectMethod.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A2%2C%22character%22%3A0%7D%5D "src/components/AzureFormComponents/AzureSelectMethod.tsx"), [`RadioGroup`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAzureFormComponents%2FAzureSelectMethod.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAzureFormComponents%2FAzureSelectMethod.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAzureFormComponents%2FAzureSelectMethod.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A2%2C%22character%22%3A0%7D%5D "src/components/AzureFormComponents/AzureSelectMethod.tsx"), [`TextField`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAzureFormComponents%2FAzureSelectMethod.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAzureFormComponents%2FAzureSelectMethod.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAzureFormComponents%2FAzureSelectMethod.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A2%2C%22character%22%3A0%7D%5D "src/components/AzureFormComponents/AzureSelectMethod.tsx") for building the UI.
- **React Hooks**: [`useEffect`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAzureFormComponents%2FAzureSelectMethod.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAzureFormComponents%2FAzureSelectMethod.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAzureFormComponents%2FAzureSelectMethod.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A10%2C%22character%22%3A0%7D%5D "src/components/AzureFormComponents/AzureSelectMethod.tsx"), [`useState`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAzureFormComponents%2FAzureSelectMethod.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAzureFormComponents%2FAzureSelectMethod.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAzureFormComponents%2FAzureSelectMethod.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A10%2C%22character%22%3A0%7D%5D "src/components/AzureFormComponents/AzureSelectMethod.tsx") for managing component state and lifecycle.
- **IconConsole**: An SVG icon imported from local assets.
- **createAccountAPI**: An API function to create an account.
- **toast**: A library for displaying notifications.
- **SectionQuestion**: A constant from utilities.

#### Interfaces
- **selectRegionProps**
  - **onNext**: Function to handle the next action.
  - **onPrev**: Function to handle the previous action.
  - **regionData**: Array of strings representing region data.
  - **aliasData**: String representing alias data.
  - **onData**: Function to handle data with parameters [`uniqueIdVal`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAzureFormComponents%2FAzureSelectMethod.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAzureFormComponents%2FAzureSelectMethod.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAzureFormComponents%2FAzureSelectMethod.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A21%2C%22character%22%3A11%7D%5D "src/components/AzureFormComponents/AzureSelectMethod.tsx") and [`externalIdVal`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAzureFormComponents%2FAzureSelectMethod.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAzureFormComponents%2FAzureSelectMethod.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAzureFormComponents%2FAzureSelectMethod.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A21%2C%22character%22%3A32%7D%5D "src/components/AzureFormComponents/AzureSelectMethod.tsx").
  - **selectionData**: Function to handle selection data with parameter [`selectedOption`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAzureFormComponents%2FAzureSelectMethod.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAzureFormComponents%2FAzureSelectMethod.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAzureFormComponents%2FAzureSelectMethod.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A22%2C%22character%22%3A18%7D%5D "src/components/AzureFormComponents/AzureSelectMethod.tsx").

#### Component: AzureSelectMethod
- **Props**: Accepts [`selectRegionProps`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAzureFormComponents%2FAzureSelectMethod.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAzureFormComponents%2FAzureSelectMethod.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAzureFormComponents%2FAzureSelectMethod.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A16%2C%22character%22%3A0%7D%5D "src/components/AzureFormComponents/AzureSelectMethod.tsx").
- **State Variables**:
  - **selectedOption**: Stores the currently selected option (number or null).
  - **isClicked**: Stores the click state (boolean or null).
- **Local Variables**:
  - **uniqueId**: Initially set to null, intended to store a unique identifier.
  - **externalId**: Initially set to null, intended to store an external identifier.
- **Constants**:
  - **question**: Retrieves a question from [`SectionQuestion`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Futils%2Fconstants.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A123%2C%22character%22%3A0%7D%5D "src/utils/constants.tsx") array at index 4.
- **Console Log**:
  - Logs the [`selectedOption`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAzureFormComponents%2FAzureSelectMethod.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAzureFormComponents%2FAzureSelectMethod.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAzureFormComponents%2FAzureSelectMethod.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A22%2C%22character%22%3A18%7D%5D "src/components/AzureFormComponents/AzureSelectMethod.tsx") to the console.

#### Pseudo Logic
1. **Initialization**:
   - Import necessary libraries and components.
   - Define the [`selectRegionProps`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAzureFormComponents%2FAzureSelectMethod.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAzureFormComponents%2FAzureSelectMethod.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAzureFormComponents%2FAzureSelectMethod.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A16%2C%22character%22%3A0%7D%5D "src/components/AzureFormComponents/AzureSelectMethod.tsx") interface for component props.
   - Define the [`AzureSelectMethod`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAzureFormComponents%2FAzureSelectMethod.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAzureFormComponents%2FAzureSelectMethod.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAzureFormComponents%2FAzureSelectMethod.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A26%2C%22character%22%3A0%7D%5D "src/components/AzureFormComponents/AzureSelectMethod.tsx") functional component.
2. **State Management**:
   - Initialize [`selectedOption`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAzureFormComponents%2FAzureSelectMethod.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAzureFormComponents%2FAzureSelectMethod.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAzureFormComponents%2FAzureSelectMethod.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A22%2C%22character%22%3A18%7D%5D "src/components/AzureFormComponents/AzureSelectMethod.tsx") state to 0.
   - Initialize [`isClicked`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAzureFormComponents%2FAzureSelectMethod.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAzureFormComponents%2FAzureSelectMethod.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAzureFormComponents%2FAzureSelectMethod.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A28%2C%22character%22%3A2%7D%5D "src/components/AzureFormComponents/AzureSelectMethod.tsx") state to null.
3. **Local Variables**:
   - Declare [`uniqueId`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAzureFormComponents%2FAzureSelectMethod.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAzureFormComponents%2FAzureSelectMethod.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAzureFormComponents%2FAzureSelectMethod.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A29%2C%22character%22%3A2%7D%5D "src/components/AzureFormComponents/AzureSelectMethod.tsx") and [`externalId`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAzureFormComponents%2FAzureSelectMethod.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAzureFormComponents%2FAzureSelectMethod.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAzureFormComponents%2FAzureSelectMethod.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A30%2C%22character%22%3A2%7D%5D "src/components/AzureFormComponents/AzureSelectMethod.tsx") as null.
4. **Constants**:
   - Retrieve a specific question from [`SectionQuestion`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Futils%2Fconstants.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A123%2C%22character%22%3A0%7D%5D "src/utils/constants.tsx").
5. **Logging**:
   - Log the [`selectedOption`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAzureFormComponents%2FAzureSelectMethod.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAzureFormComponents%2FAzureSelectMethod.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAzureFormComponents%2FAzureSelectMethod.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A22%2C%22character%22%3A18%7D%5D "src/components/AzureFormComponents/AzureSelectMethod.tsx") to the console.
6. **Commented Code**:
   - There is a commented-out function `uniqueIdAPI` which is intended to call [`createAccountAPI`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FcreateAccount.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A22%2C%22character%22%3A0%7D%5D "src/apis/createAccount.tsx") with parameters and store the response in `unique_id`.

#### Example Usage
```tsx
<AzureSelectMethod
  onNext={handleNext}
  onPrev={handlePrev}
  regionData={regionDataArray}
  aliasData={aliasString}
  onData={handleData}
  selectionData={handleSelection}
/>
```

This documentation provides a detailed overview of the `AzureSelectMethod.tsx` file, including its imports, interfaces, component structure, state management, and pseudo logic.

sumitchohan: generate detailed documentation for current code file including all methods, fields, properties and summary of their implementation including pseudo logic

GitHub Copilot: ### AzureSelectRegions.tsx Documentation

#### Summary
This file defines a React functional component [`AzureSelectRegions`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAzureFormComponents%2FAzureSelectRegions.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAzureFormComponents%2FAzureSelectRegions.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAzureFormComponents%2FAzureSelectRegions.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A28%2C%22character%22%3A2%7D%5D "src/components/AzureFormComponents/AzureSelectRegions.tsx") which is used to select Azure regions. It includes various UI elements such as checkboxes, radio buttons, and text fields, and handles user interactions to manage the state of selected regions.

#### Imports
- **FontAwesome Icons**: Used for rendering icons.
  ```tsx
  import { faMagnifyingGlass, faWindowRestore } from "@fortawesome/free-solid-svg-icons";
  import { FontAwesomeIcon } from "@fortawesome/react-fontawesome";
  ```
- **Material UI Components**: Used for rendering form controls and input elements.
  ```tsx
  import { Checkbox, FormControl, FormControlLabel, FormGroup, InputAdornment, Radio, RadioGroup, TextField } from "@mui/material";
  ```
- **React Hooks**: Used for managing component state and side effects.
  ```tsx
  import { useEffect, useState } from "react";
  ```
- **Custom Components and Utilities**:
  ```tsx
  import SelectCloud from "../selectCloud";
  import { useNavigate } from "react-router-dom";
  import toast from "react-hot-toast";
  import { SectionQuestion } from "../../utils/constants";
  ```

#### Interface
- **AzureSelectRegionsProps**: Defines the properties expected by the `AzureSelectRegions` component.
  ```tsx
  interface AzureSelectRegionsProps {
    onNext: () => void;
    onPrev: () => void;
    onData: (regionData: string[]) => void;
    formData: { alias: string; region: string[] };
  }
  ```

#### Component
- **AzureSelectRegions**: The main functional component.
  ```tsx
  const AzureSelectRegions: React.FC<AzureSelectRegionsProps> = (props) => {
  ```

#### State Variables
- **searchQuery**: Manages the state of the search input field.
  ```tsx
  const [searchQuery, setSearchQuery] = useState<string>("");
  ```
- **currentQuestion**: Tracks the current question index.
  ```tsx
  const [currentQuestion, setCurrentQuestion] = useState<number>(0);
  ```
- **selectedOption**: Stores the selected regions from the form data.
  ```tsx
  const [selectedOption, setSelectedOption] = useState<string[]>(props.formData.region);
  ```
- **selectedOptions**: Manages the state of selected options.
  ```tsx
  const [selectedOptions, setSelectedOptions] = useState<string[]>([]);
  ```
- **buttonClicked**: Tracks whether a button has been clicked.
  ```tsx
  const [buttonClicked, setButtonClicked] = useState<boolean>(false);
  ```

#### useEffect Hook
- **useEffect**: Synchronizes the [`selectedOptions`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAzureFormComponents%2FAzureSelectRegions.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAzureFormComponents%2FAzureSelectRegions.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAzureFormComponents%2FAzureSelectRegions.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A34%2C%22character%22%3A4%7D%5D "src/components/AzureFormComponents/AzureSelectRegions.tsx") state with the [`formData.region`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAzureFormComponents%2FAzureSelectRegions.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAzureFormComponents%2FAzureSelectRegions.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FAzureFormComponents%2FAzureSelectRegions.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A25%2C%22character%22%3A4%7D%5D "src/components/AzureFormComponents/AzureSelectRegions.tsx") prop whenever it changes.
  ```tsx
  useEffect(() => {
    setSelectedOptions(props.formData.region);
  }, [props.formData.region]);
  ```

### Pseudo Logic
1. **Imports**: Import necessary libraries and components.
2. **Interface Definition**: Define the `AzureSelectRegionsProps` interface to specify the expected props.
3. **Component Definition**: Define the `AzureSelectRegions` functional component.
4. **State Initialization**: Initialize state variables using React hooks.
5. **Effect Hook**: Use `useEffect` to update `selectedOptions` when `formData.region` changes.
6. **Render UI**: Render the UI elements (not shown in the provided code) which include checkboxes, radio buttons, text fields, and other form controls.
7. **Event Handlers**: Define event handlers for user interactions (not shown in the provided code).

This documentation provides a detailed overview of the `AzureSelectRegions.tsx` file, including its imports, interface, component, state variables, and pseudo logic.

sumitchohan: generate detailed documentation for current code file including all methods, fields, properties and summary of their implementation including pseudo logic

GitHub Copilot: ### allCostSavingTable.tsx Documentation

#### Summary
This file defines a React functional component [`CostSavingsCard`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FDashboardChartComponent%2FallCostSavingTable.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FDashboardChartComponent%2FallCostSavingTable.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FDashboardChartComponent%2FallCostSavingTable.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A8%2C%22character%22%3A0%7D%5D "src/components/DashboardChartComponent/allCostSavingTable.tsx") which displays a table of policies with the highest cost savings. The component includes a tooltip for additional information and uses various CSS classes for styling.

#### Imports
- **icon**: An SVG icon used for the tooltip.
  ```tsx
  import icon from "../../assets/icons/icon-tool-tip.svg";
  ```
- **Tippy**: A React component for tooltips.
  ```tsx
  import Tippy from "@tippyjs/react";
  ```
- **iconChangeUp**: An SVG icon indicating an increase.
  ```tsx
  import iconChangeUp from "../../assets/icons/icon-change-up.svg";
  ```
- **iconChangeDown**: An SVG icon indicating a decrease.
  ```tsx
  import iconChangeDown from "../../assets/icons/icon-change-down.svg";
  ```
- **CSS**: Custom CSS for styling the component.
  ```tsx
  import "../../components-css/costSavingsCard.css";
  ```

#### Interface
- **CostSavingsCardProps**: An empty interface indicating that the `CostSavingsCard` component does not expect any props.
  ```tsx
  interface CostSavingsCardProps {}
  ```

#### Component
- **CostSavingsCard**: The main functional component.
  ```tsx
  const CostSavingsCard: React.FC<CostSavingsCardProps> = () => {
  ```

#### Constants
- **cellCSS**: A string constant for common CSS classes applied to table cells.
  ```tsx
  const cellCSS = "py-2 font-size-cost-card pr-2 text-sm align-top";
  ```

#### JSX Structure
- **Main Container**: A `div` element with various CSS classes for layout and styling.
  ```tsx
  <div className="w-1/2 h-96 p-5 my-5 shadow-xl border relative overflow-hidden bg-white">
  ```
- **Header**: An `h1` element displaying the title.
  ```tsx
  <h1 className="text-base font-poppins font-medium flex font-size-cost-card ">
    Policies with highest Cost Savings
  </h1>
  ```
- **Tooltip**: A `div` containing a tooltip with an icon.
  ```tsx
  <div className="absolute top-2 right-2 w-10 h-10 p-1 rounded-full group text-sm ">
    <Tippy
      content={"Policies with highest Cost Savings"}
      placement="top"
      className="bg-black text-white text-xs p-2 rounded shadow-lg"
    >
      <img src={icon} alt="Icon" className="w-10 h-10 p-1 rounded-full" />
    </Tippy>
  </div>
  ```
- **Table**: A `table` element displaying policy data.
  ```tsx
  <table className="table-auto w-full relative mt-6  ">
    <thead>
      <tr className="border-b">
        <td className=" font-semibold font-size-cost-card w-2/5 text-sm ">
          Policy Name
        </td>
        <td className=" font-semibold font-size-cost-card w-1/5 text-sm ">
          Policy Type
        </td>
        <td className=" font-semibold font-size-cost-card w-1/5 text-sm">
          Cost Saving
        </td>
      </tr>
    </thead>
  ```

### Pseudo Logic
1. **Imports**: Import necessary libraries, icons, and CSS files.
2. **Interface Definition**: Define the `CostSavingsCardProps` interface (currently empty).
3. **Component Definition**: Define the `CostSavingsCard` functional component.
4. **Constants**: Define the `cellCSS` constant for common table cell styling.
5. **Render UI**:
   - Create a main container `div` with specific CSS classes.
   - Add a header `h1` with the title "Policies with highest Cost Savings".
   - Add a tooltip using the `Tippy` component, containing an icon.
   - Create a table with a header row defining columns for "Policy Name", "Policy Type", and "Cost Saving".

This documentation provides a detailed overview of the `allCostSavingTable.tsx` file, including its imports, interface, component, constants, and pseudo logic.

sumitchohan: generate detailed documentation for current code file including all methods, fields, properties and summary of their implementation including pseudo logic

GitHub Copilot: ### dashboard.css Documentation

#### Summary
This CSS file provides styling rules for various elements within a dashboard interface. It includes styles for general elements, containers, titles, labels, loaders, and custom tooltips.

#### CSS Rules

1. **Universal Selector for `.db` Class**
   ```css
   .db * {
       border: 1px solid red;
   }
   ```
   - **Description**: Applies a red border to all elements within any element that has the `db` class.
   - **Purpose**: Likely used for debugging to visualize element boundaries.

2. **[`.row-container`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FDashboardChartComponent%2Fdashboard.css%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A4%2C%22character%22%3A0%7D%5D "src/components/DashboardChartComponent/dashboard.css") Class**
   ```css
   .row-container {
       width: 98%;
       margin: auto;
   }
   ```
   - **Description**: Sets the width of the container to 98% and centers it horizontally.
   - **Purpose**: Ensures the container takes up most of the available width while maintaining some margin on the sides.

3. **[`.row-title`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FDashboardChartComponent%2Fdashboard.css%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A9%2C%22character%22%3A0%7D%5D "src/components/DashboardChartComponent/dashboard.css") Class**
   ```css
   .row-title {
       margin-bottom: 30px;
   }
   ```
   - **Description**: Adds a 30px margin to the bottom of the element.
   - **Purpose**: Provides spacing below titles to separate them from subsequent content.

4. **[`.dashboard-label`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FDashboardChartComponent%2Fdashboard.css%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A13%2C%22character%22%3A0%7D%5D "src/components/DashboardChartComponent/dashboard.css") Class**
   ```css
   .dashboard-label {
       font-size: 24px;
       font-weight: 600;
   }
   ```
   - **Description**: Sets the font size to 24px and the font weight to 600 (semi-bold).
   - **Purpose**: Styles labels within the dashboard to be prominent and easily readable.

5. **[`.mini-loader`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FDashboardChartComponent%2Fdashboard.css%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A18%2C%22character%22%3A0%7D%5D "src/components/DashboardChartComponent/dashboard.css") Class**
   ```css
   .mini-loader {
       width: 20px;
       height: 20px;
   }
   ```
   - **Description**: Sets the width and height of the loader to 20px.
   - **Purpose**: Defines the size of a small loading indicator.

6. **[`select`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FDashboardChartComponent%2Fdashboard.css%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A23%2C%22character%22%3A0%7D%5D "src/components/DashboardChartComponent/dashboard.css") Element**
   ```css
   select {
       border: none;
       min-width: 150px;
       text-align: right;
       padding: 1px 5px;
   }
   ```
   - **Description**: Removes the border, sets a minimum width of 150px, aligns text to the right, and adds padding.
   - **Purpose**: Styles dropdown select elements to have a clean appearance and sufficient clickable area.

7. **`select > option` Element**
   ```css
   select > option {
       padding: 5px;
       text-align: left;
   }
   ```
   - **Description**: Adds padding and left-aligns text within options of a select element.
   - **Purpose**: Ensures options within dropdowns are spaced out and aligned for readability.

8. **[`.custom-tool-tip`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FDashboardChartComponent%2Fdashboard.css%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A37%2C%22character%22%3A0%7D%5D "src/components/DashboardChartComponent/dashboard.css") Class**
   ```css
   .custom-tool-tip {
       width: 300px;
   }
   ```
   - **Description**: Sets the width of custom tooltips to 300px.
   - **Purpose**: Defines a fixed width for tooltips to ensure consistent appearance.

### Pseudo Logic
1. **Universal Selector**: Apply a red border to all elements within `.db` for debugging.
2. **Container Styling**: Style [`.row-container`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FDashboardChartComponent%2Fdashboard.css%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A4%2C%22character%22%3A0%7D%5D "src/components/DashboardChartComponent/dashboard.css") to be almost full-width and centered.
3. **Title Spacing**: Add bottom margin to [`.row-title`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FDashboardChartComponent%2Fdashboard.css%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A9%2C%22character%22%3A0%7D%5D "src/components/DashboardChartComponent/dashboard.css") for spacing.
4. **Label Styling**: Style [`.dashboard-label`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FDashboardChartComponent%2Fdashboard.css%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A13%2C%22character%22%3A0%7D%5D "src/components/DashboardChartComponent/dashboard.css") for prominence.
5. **Loader Size**: Define size for [`.mini-loader`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FDashboardChartComponent%2Fdashboard.css%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A18%2C%22character%22%3A0%7D%5D "src/components/DashboardChartComponent/dashboard.css").
6. **Select Element Styling**: Style [`select`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FDashboardChartComponent%2Fdashboard.css%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A23%2C%22character%22%3A0%7D%5D "src/components/DashboardChartComponent/dashboard.css") elements for a clean look.
7. **Option Element Styling**: Style [`select > option`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FDashboardChartComponent%2Fdashboard.css%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A23%2C%22character%22%3A0%7D%5D "src/components/DashboardChartComponent/dashboard.css") for readability.
8. **Tooltip Width**: Set a fixed width for [`.custom-tool-tip`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FDashboardChartComponent%2Fdashboard.css%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A37%2C%22character%22%3A0%7D%5D "src/components/DashboardChartComponent/dashboard.css").

This documentation provides a detailed overview of the [`dashboard.css`](command:_github.copilot.openRelativePath?%5B%7B%22scheme%22%3A%22file%22%2C%22authority%22%3A%22%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FDashboardChartComponent%2Fdashboard.css%22%2C%22query%22%3A%22%22%2C%22fragment%22%3A%22%22%7D%5D "/Users/sumit/Downloads/Frontend/NAGISVPR/src/components/DashboardChartComponent/dashboard.css") file, including descriptions and purposes for each CSS rule, as well as a pseudo logic summary.

sumitchohan: generate detailed documentation for current code file including all methods, fields, properties and summary of their implementation including pseudo logic

GitHub Copilot: ### Documentation for [`highestPotentialCostSaving.tsx`](command:_github.copilot.openRelativePath?%5B%7B%22scheme%22%3A%22file%22%2C%22authority%22%3A%22%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FDashboardChartComponent%2FhighestPotentialCostSaving.tsx%22%2C%22query%22%3A%22%22%2C%22fragment%22%3A%22%22%7D%5D "/Users/sumit/Downloads/Frontend/NAGISVPR/src/components/DashboardChartComponent/highestPotentialCostSaving.tsx")

#### Summary
This file defines a React functional component named [`HighestPotentialCostSaving`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FDashboardChartComponent%2FhighestPotentialCostSaving.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FDashboardChartComponent%2FhighestPotentialCostSaving.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FDashboardChartComponent%2FhighestPotentialCostSaving.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A7%2C%22character%22%3A0%7D%5D "src/components/DashboardChartComponent/highestPotentialCostSaving.tsx"). The component displays a section with a title, a tooltip, and a pie chart image. It also lists some items with colored indicators. The component uses Tailwind CSS for styling and Tippy.js for the tooltip functionality.

#### Imports
- [`React`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FDashboardChartComponent%2FhighestPotentialCostSaving.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FDashboardChartComponent%2FhighestPotentialCostSaving.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FDashboardChartComponent%2FhighestPotentialCostSaving.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A0%2C%22character%22%3A0%7D%5D "src/components/DashboardChartComponent/highestPotentialCostSaving.tsx"): The core React library for building user interfaces.
- [`piechart`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FDashboardChartComponent%2FhighestPotentialCostSaving.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FDashboardChartComponent%2FhighestPotentialCostSaving.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FDashboardChartComponent%2FhighestPotentialCostSaving.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A3%2C%22character%22%3A0%7D%5D "src/components/DashboardChartComponent/highestPotentialCostSaving.tsx"): An image asset imported from the local file system.
- [`Tippy`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FDashboardChartComponent%2FhighestPotentialCostSaving.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FDashboardChartComponent%2FhighestPotentialCostSaving.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FDashboardChartComponent%2FhighestPotentialCostSaving.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A4%2C%22character%22%3A0%7D%5D "src/components/DashboardChartComponent/highestPotentialCostSaving.tsx"): A React wrapper for the Tippy.js library, used for tooltips.
- [`icon`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FDashboardChartComponent%2FhighestPotentialCostSaving.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FDashboardChartComponent%2FhighestPotentialCostSaving.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FDashboardChartComponent%2FhighestPotentialCostSaving.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A5%2C%22character%22%3A0%7D%5D "src/components/DashboardChartComponent/highestPotentialCostSaving.tsx"): An image asset for the tooltip icon, imported from the local file system.

#### Interface
- [`HighestPotentialCostSavingProps`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FDashboardChartComponent%2FhighestPotentialCostSaving.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FDashboardChartComponent%2FhighestPotentialCostSaving.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FDashboardChartComponent%2FhighestPotentialCostSaving.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A2%2C%22character%22%3A0%7D%5D "src/components/DashboardChartComponent/highestPotentialCostSaving.tsx"): An empty interface that defines the props for the [`HighestPotentialCostSaving`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FDashboardChartComponent%2FhighestPotentialCostSaving.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FDashboardChartComponent%2FhighestPotentialCostSaving.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FDashboardChartComponent%2FhighestPotentialCostSaving.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A7%2C%22character%22%3A0%7D%5D "src/components/DashboardChartComponent/highestPotentialCostSaving.tsx") component. Currently, it does not accept any props.

#### Component: [`HighestPotentialCostSaving`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FDashboardChartComponent%2FhighestPotentialCostSaving.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FDashboardChartComponent%2FhighestPotentialCostSaving.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FDashboardChartComponent%2FhighestPotentialCostSaving.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A7%2C%22character%22%3A0%7D%5D "src/components/DashboardChartComponent/highestPotentialCostSaving.tsx")
- **Type**: Functional Component
- **Props**: [`HighestPotentialCostSavingProps`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FDashboardChartComponent%2FhighestPotentialCostSaving.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FDashboardChartComponent%2FhighestPotentialCostSaving.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FDashboardChartComponent%2FhighestPotentialCostSaving.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A2%2C%22character%22%3A0%7D%5D "src/components/DashboardChartComponent/highestPotentialCostSaving.tsx") (currently empty)

##### JSX Structure
1. **Container Div**:
   - Class: `w-1/2 h-96 p-5 my-5 shadow-xl border relative bg-white`
   - Description: A container with specific width, height, padding, margin, shadow, border, and background color.

2. **Header Section**:
   - **Title Div**:
     - Class: `flex justify-between items-center`
     - Description: A flex container that justifies content between and aligns items center.
     - **Text Div**:
       - Class: `text-base font-poppins font-medium`
       - Description: Displays the title "Account with highest Potential Cost saving (in %)".
     - **Tooltip Div**:
       - Contains a [`Tippy`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FDashboardChartComponent%2FhighestPotentialCostSaving.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FDashboardChartComponent%2FhighestPotentialCostSaving.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FDashboardChartComponent%2FhighestPotentialCostSaving.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A4%2C%22character%22%3A0%7D%5D "src/components/DashboardChartComponent/highestPotentialCostSaving.tsx") component with the tooltip content "Account with highest Potential Cost saving".
       - **Icon Image**:
         - Source: [`icon`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FDashboardChartComponent%2FhighestPotentialCostSaving.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FDashboardChartComponent%2FhighestPotentialCostSaving.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FDashboardChartComponent%2FhighestPotentialCostSaving.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A5%2C%22character%22%3A0%7D%5D "src/components/DashboardChartComponent/highestPotentialCostSaving.tsx")
         - Class: `p-1 rounded-full`
         - Description: An image with padding and rounded corners.

3. **Content Section**:
   - **Pie Chart Image**:
     - Source: [`piechart`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FDashboardChartComponent%2FhighestPotentialCostSaving.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FDashboardChartComponent%2FhighestPotentialCostSaving.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FDashboardChartComponent%2FhighestPotentialCostSaving.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A3%2C%22character%22%3A0%7D%5D "src/components/DashboardChartComponent/highestPotentialCostSaving.tsx")
     - Class: `flex mt-8 ml-10`
     - Description: Displays the pie chart image.
   - **List of Items**:
     - **Item Container**:
       - Class: `flex items-center`
       - Description: A flex container that aligns items center.
       - **Item 1**:
         - **Indicator Div**:
           - Class: `bg-ntest-100 w-4 h-4 rounded-full mx-4`
           - Description: A small colored circle.
         - **Text Div**:
           - Description: Displays the text "app_dev_crm".
       - **Item 2**:
         - **Indicator Div**:
           - Class: `bg-ntest-200 w-4 h-4 rounded-full mx-4`
           - Description: A small colored circle.
         - **Text Div**:
           - Description: Displays the text "app_live_hris".

#### Pseudo Logic
1. **Render Container Div**:
   - Apply the specified classes for styling.
2. **Render Header Section**:
   - Create a flex container.
   - Add a title with specific text and styling.
   - Add a tooltip with an icon image.
3. **Render Content Section**:
   - Add the pie chart image.
   - Create a list of items with colored indicators and corresponding text.

#### Code
```tsx
import React from "react";

interface HighestPotentialCostSavingProps {}
import piechart from "../../assets/piechart-ss.png";
import Tippy from "@tippyjs/react";
import icon from "../../assets/icons/icon-tool-tip.svg";

const HighestPotentialCostSaving: React.FC<HighestPotentialCostSavingProps> = () => {
  return (
    <div className="w-1/2 h-96 p-5 my-5 shadow-xl border relative bg-white">
      <div className="flex justify-between items-center">
        <div className="text-base font-poppins font-medium">
          Account with highest Potential Cost saving{" "}
          <span className="">(in %)</span>{" "}
        </div>
        <div className="">
          <Tippy content={"Account with highest Potential Cost saving "} placement="top">
            <img src={icon} alt="Icon" className="p-1 rounded-full" />
          </Tippy>
        </div>
      </div>

      <div className="flex mt-8 ml-10">
        <img src={piechart} className=""></img>
        <div className="flex items-center">
          <div>
            <div className="flex items-center py-1">
              <div className="bg-ntest-100 w-4 h-4 rounded-full mx-4"></div>
              <div>app_dev_crm</div>
            </div>
            <div className="flex items-center py-1">
              <div className="bg-ntest-200 w-4 h-4 rounded-full mx-4"></div>
              <div>app_live_hris</div>
            </div>
            <div className="flex items-center py-1">
              <div className="bg-ntest-300 w-4 h-4 rounded-full mx-4"></div>
```

This documentation provides a detailed overview of the `highestPotentialCostSaving.tsx` file, including its structure, components, and pseudo logic.

sumitchohan: generate detailed documentation for current code file including all methods, fields, properties and summary of their implementation including pseudo logic


GitHub Copilot: ### Documentation for [`nonCompliantPoliciesChart.tsx`](command:_github.copilot.openRelativePath?%5B%7B%22scheme%22%3A%22file%22%2C%22authority%22%3A%22%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FDashboardChartComponent%2FnonCompliantPoliciesChart.tsx%22%2C%22query%22%3A%22%22%2C%22fragment%22%3A%22%22%7D%5D "/Users/sumit/Downloads/Frontend/NAGISVPR/src/components/DashboardChartComponent/nonCompliantPoliciesChart.tsx")

#### Summary
This file defines a React component that renders a bar chart using the `recharts` library. The chart visualizes data related to non-compliant policies, such as unattached EBS volumes and low EC2 CPU utilization. The component also includes tooltips for additional information.

#### Imports
- **Tippy**: A React component for tooltips.
- **React**: The core library for building user interfaces.
- **icon**: An SVG icon for tooltips.
- **recharts**: A charting library for React.

#### Interfaces
- **DataItem**
  - **name**: `string` - The name of the policy.
  - **uv**: `number` - A numerical value representing some metric (e.g., usage value).
  - **pv**: `number` - Another numerical value representing a different metric (e.g., policy violations).
  - **amt**: `number` - An additional numerical value (e.g., amount).

#### Data
- **data**: [`DataItem[]`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FDashboardChartComponent%2FnonCompliantPoliciesChart.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FDashboardChartComponent%2FnonCompliantPoliciesChart.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FDashboardChartComponent%2FnonCompliantPoliciesChart.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A16%2C%22character%22%3A0%7D%5D "src/components/DashboardChartComponent/nonCompliantPoliciesChart.tsx") - An array of objects conforming to the [`DataItem`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FDashboardChartComponent%2FnonCompliantPoliciesChart.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FDashboardChartComponent%2FnonCompliantPoliciesChart.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FDashboardChartComponent%2FnonCompliantPoliciesChart.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A16%2C%22character%22%3A0%7D%5D "src/components/DashboardChartComponent/nonCompliantPoliciesChart.tsx") interface. Each object represents a different non-compliant policy with various metrics.

#### Functions
- **getIntroOfPage**
  - **Parameters**: 
    - [`label`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FDashboardChartComponent%2FnonCompliantPoliciesChart.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FDashboardChartComponent%2FnonCompliantPoliciesChart.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FDashboardChartComponent%2FnonCompliantPoliciesChart.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A31%2C%22character%22%3A24%7D%5D "src/components/DashboardChartComponent/nonCompliantPoliciesChart.tsx"): `string` - The label of the page.
  - **Returns**: `string` - A string representing the introductory text for the given label.
  - **Pseudo Logic**:
    1. Check the value of [`label`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FDashboardChartComponent%2FnonCompliantPoliciesChart.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FDashboardChartComponent%2FnonCompliantPoliciesChart.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FDashboardChartComponent%2FnonCompliantPoliciesChart.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A31%2C%22character%22%3A24%7D%5D "src/components/DashboardChartComponent/nonCompliantPoliciesChart.tsx").
    2. Return a corresponding string based on the label value.
    3. If the label is "Page A", return "48".
    4. If the label is "Page B", return "40".
    5. If the label is "Page C", return "35".
    6. If the label is "Page D", return "26".

#### Pseudo Logic for the Component (not fully shown in the excerpt)
1. **Import necessary libraries and assets**.
2. **Define the [`DataItem`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FDashboardChartComponent%2FnonCompliantPoliciesChart.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FDashboardChartComponent%2FnonCompliantPoliciesChart.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FDashboardChartComponent%2FnonCompliantPoliciesChart.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A16%2C%22character%22%3A0%7D%5D "src/components/DashboardChartComponent/nonCompliantPoliciesChart.tsx") interface** to structure the data.
3. **Create a data array** with objects conforming to the [`DataItem`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FDashboardChartComponent%2FnonCompliantPoliciesChart.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FDashboardChartComponent%2FnonCompliantPoliciesChart.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FDashboardChartComponent%2FnonCompliantPoliciesChart.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A16%2C%22character%22%3A0%7D%5D "src/components/DashboardChartComponent/nonCompliantPoliciesChart.tsx") interface.
4. **Define the [`getIntroOfPage`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FDashboardChartComponent%2FnonCompliantPoliciesChart.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FDashboardChartComponent%2FnonCompliantPoliciesChart.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FDashboardChartComponent%2FnonCompliantPoliciesChart.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A31%2C%22character%22%3A0%7D%5D "src/components/DashboardChartComponent/nonCompliantPoliciesChart.tsx") function** to provide introductory text based on the label.
5. **Render the bar chart** using `recharts` components:
   - Use [`ResponsiveContainer`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FDashboardChartComponent%2FnonCompliantPoliciesChart.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FDashboardChartComponent%2FnonCompliantPoliciesChart.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FDashboardChartComponent%2FnonCompliantPoliciesChart.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A4%2C%22character%22%3A0%7D%5D "src/components/DashboardChartComponent/nonCompliantPoliciesChart.tsx") to make the chart responsive.
   - Use [`BarChart`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FDashboardChartComponent%2FnonCompliantPoliciesChart.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FDashboardChartComponent%2FnonCompliantPoliciesChart.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FDashboardChartComponent%2FnonCompliantPoliciesChart.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A4%2C%22character%22%3A0%7D%5D "src/components/DashboardChartComponent/nonCompliantPoliciesChart.tsx") to define the chart type.
   - Add [`XAxis`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FDashboardChartComponent%2FnonCompliantPoliciesChart.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FDashboardChartComponent%2FnonCompliantPoliciesChart.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FDashboardChartComponent%2FnonCompliantPoliciesChart.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A4%2C%22character%22%3A0%7D%5D "src/components/DashboardChartComponent/nonCompliantPoliciesChart.tsx") and [`YAxis`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FDashboardChartComponent%2FnonCompliantPoliciesChart.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FDashboardChartComponent%2FnonCompliantPoliciesChart.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FDashboardChartComponent%2FnonCompliantPoliciesChart.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A4%2C%22character%22%3A0%7D%5D "src/components/DashboardChartComponent/nonCompliantPoliciesChart.tsx") for the axes.
   - Add [`CartesianGrid`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FDashboardChartComponent%2FnonCompliantPoliciesChart.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FDashboardChartComponent%2FnonCompliantPoliciesChart.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FDashboardChartComponent%2FnonCompliantPoliciesChart.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A4%2C%22character%22%3A0%7D%5D "src/components/DashboardChartComponent/nonCompliantPoliciesChart.tsx") for the grid lines.
   - Add [`Tooltip`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FDashboardChartComponent%2FnonCompliantPoliciesChart.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FDashboardChartComponent%2FnonCompliantPoliciesChart.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FDashboardChartComponent%2FnonCompliantPoliciesChart.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A4%2C%22character%22%3A0%7D%5D "src/components/DashboardChartComponent/nonCompliantPoliciesChart.tsx") and [`Legend`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FDashboardChartComponent%2FnonCompliantPoliciesChart.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FDashboardChartComponent%2FnonCompliantPoliciesChart.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FDashboardChartComponent%2FnonCompliantPoliciesChart.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A4%2C%22character%22%3A0%7D%5D "src/components/DashboardChartComponent/nonCompliantPoliciesChart.tsx") for additional information.
   - Use [`Bar`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FDashboardChartComponent%2FnonCompliantPoliciesChart.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FDashboardChartComponent%2FnonCompliantPoliciesChart.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FDashboardChartComponent%2FnonCompliantPoliciesChart.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A4%2C%22character%22%3A0%7D%5D "src/components/DashboardChartComponent/nonCompliantPoliciesChart.tsx") to define the bars in the chart.
6. **Integrate tooltips** using the [`Tippy`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FDashboardChartComponent%2FnonCompliantPoliciesChart.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FDashboardChartComponent%2FnonCompliantPoliciesChart.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FDashboardChartComponent%2FnonCompliantPoliciesChart.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A0%2C%22character%22%3A0%7D%5D "src/components/DashboardChartComponent/nonCompliantPoliciesChart.tsx") component for additional information on hover.

#### Example Usage
```tsx
import React from "react";
import NonCompliantPoliciesChart from "./nonCompliantPoliciesChart";

const App = () => (
  <div>
    <h1>Non-Compliant Policies</h1>
    <NonCompliantPoliciesChart />
  </div>
);

export default App;
```

This documentation provides a comprehensive overview of the `nonCompliantPoliciesChart.tsx` file, detailing its structure, data, and functionality.

sumitchohan: generate detailed documentation for current code file including all methods, fields, properties and summary of their implementation including pseudo logic

GitHub Copilot: ### Documentation for [`policyComplianceChart.tsx`](command:_github.copilot.openRelativePath?%5B%7B%22scheme%22%3A%22file%22%2C%22authority%22%3A%22%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FDashboardChartComponent%2FpolicyComplianceChart.tsx%22%2C%22query%22%3A%22%22%2C%22fragment%22%3A%22%22%7D%5D "/Users/sumit/Downloads/Frontend/NAGISVPR/src/components/DashboardChartComponent/policyComplianceChart.tsx")

#### Summary
This file defines a React functional component [`PolicyComplianceChart`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FDashboardChartComponent%2FpolicyComplianceChart.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FDashboardChartComponent%2FpolicyComplianceChart.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FDashboardChartComponent%2FpolicyComplianceChart.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A23%2C%22character%22%3A0%7D%5D "src/components/DashboardChartComponent/policyComplianceChart.tsx") that renders a bar chart using the `recharts` library. The chart displays compliance data for various applications, showing both compliant and noncompliant counts.

#### Imports
- **Tippy**: A tooltip library for React.
- **React**: Core React library and hooks.
- **Recharts Components**: Various components from the `recharts` library to build the bar chart.
- **CSS**: Custom styles for the dashboard.
- **Icon**: An SVG icon for tooltips.
- **FontAwesome**: An icon library (though [`height`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FDashboardChartComponent%2FpolicyComplianceChart.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FDashboardChartComponent%2FpolicyComplianceChart.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FDashboardChartComponent%2FpolicyComplianceChart.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A13%2C%22character%22%3A0%7D%5D "src/components/DashboardChartComponent/policyComplianceChart.tsx") from `faAddressBook` seems unused).

#### Interfaces
- **PolicyComplianceChartProps**: An empty interface, indicating no props are currently required for this component.

#### Constants
- **chartData**: An array of objects representing the compliance data for different applications. Each object contains:
  - [`name`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FDashboardChartComponent%2FpolicyComplianceChart.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FDashboardChartComponent%2FpolicyComplianceChart.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FDashboardChartComponent%2FpolicyComplianceChart.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A17%2C%22character%22%3A4%7D%5D "src/components/DashboardChartComponent/policyComplianceChart.tsx"): The name of the application.
  - [`compliant`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FDashboardChartComponent%2FpolicyComplianceChart.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FDashboardChartComponent%2FpolicyComplianceChart.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FDashboardChartComponent%2FpolicyComplianceChart.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A17%2C%22character%22%3A21%7D%5D "src/components/DashboardChartComponent/policyComplianceChart.tsx"): The number of compliant items.
  - [`noncompliant`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FDashboardChartComponent%2FpolicyComplianceChart.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FDashboardChartComponent%2FpolicyComplianceChart.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FDashboardChartComponent%2FpolicyComplianceChart.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A17%2C%22character%22%3A35%7D%5D "src/components/DashboardChartComponent/policyComplianceChart.tsx"): The number of noncompliant items.

#### Component: [`PolicyComplianceChart`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FDashboardChartComponent%2FpolicyComplianceChart.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FDashboardChartComponent%2FpolicyComplianceChart.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FDashboardChartComponent%2FpolicyComplianceChart.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A23%2C%22character%22%3A0%7D%5D "src/components/DashboardChartComponent/policyComplianceChart.tsx")
A functional component that renders a bar chart with compliance data.

##### State
- **services**: A state variable initialized to a set containing "noncompliant" and "compliant". This set determines which bars are displayed in the chart.

##### Constants
- **COLORS**: An array of color codes used to fill the bars in the chart.

##### Methods
- **applyBars**: A function that maps over the [`services`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FDashboardChartComponent%2FpolicyComplianceChart.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FDashboardChartComponent%2FpolicyComplianceChart.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FDashboardChartComponent%2FpolicyComplianceChart.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A24%2C%22character%22%3A2%7D%5D "src/components/DashboardChartComponent/policyComplianceChart.tsx") set and returns an array of [`Bar`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FDashboardChartComponent%2FpolicyComplianceChart.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FDashboardChartComponent%2FpolicyComplianceChart.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FDashboardChartComponent%2FpolicyComplianceChart.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A2%2C%22character%22%3A0%7D%5D "src/components/DashboardChartComponent/policyComplianceChart.tsx") components. Each [`Bar`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FDashboardChartComponent%2FpolicyComplianceChart.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FDashboardChartComponent%2FpolicyComplianceChart.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FDashboardChartComponent%2FpolicyComplianceChart.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A2%2C%22character%22%3A0%7D%5D "src/components/DashboardChartComponent/policyComplianceChart.tsx") component is configured with:
  - [`key`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FDashboardChartComponent%2FpolicyComplianceChart.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FDashboardChartComponent%2FpolicyComplianceChart.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FDashboardChartComponent%2FpolicyComplianceChart.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A32%2C%22character%22%3A8%7D%5D "src/components/DashboardChartComponent/policyComplianceChart.tsx"): A unique key for each bar.
  - [`dataKey`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FDashboardChartComponent%2FpolicyComplianceChart.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FDashboardChartComponent%2FpolicyComplianceChart.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FDashboardChartComponent%2FpolicyComplianceChart.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A33%2C%22character%22%3A8%7D%5D "src/components/DashboardChartComponent/policyComplianceChart.tsx"): The data key (either "compliant" or "noncompliant").
  - [`stackId`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FDashboardChartComponent%2FpolicyComplianceChart.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FDashboardChartComponent%2FpolicyComplianceChart.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FDashboardChartComponent%2FpolicyComplianceChart.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A34%2C%22character%22%3A8%7D%5D "src/components/DashboardChartComponent/policyComplianceChart.tsx"): A common stack ID to stack bars on top of each other.
  - [`isAnimationActive`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FDashboardChartComponent%2FpolicyComplianceChart.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FDashboardChartComponent%2FpolicyComplianceChart.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FDashboardChartComponent%2FpolicyComplianceChart.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A36%2C%22character%22%3A8%7D%5D "src/components/DashboardChartComponent/policyComplianceChart.tsx"): A boolean to enable animations.
  - [`fill`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FDashboardChartComponent%2FpolicyComplianceChart.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FDashboardChartComponent%2FpolicyComplianceChart.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FDashboardChartComponent%2FpolicyComplianceChart.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A37%2C%22character%22%3A8%7D%5D "src/components/DashboardChartComponent/policyComplianceChart.tsx"): The color of the bar, chosen from the [`COLORS`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FDashboardChartComponent%2FpolicyComplianceChart.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FDashboardChartComponent%2FpolicyComplianceChart.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FDashboardChartComponent%2FpolicyComplianceChart.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A27%2C%22character%22%3A2%7D%5D "src/components/DashboardChartComponent/policyComplianceChart.tsx") array.

#### Pseudo Logic
1. **Initialize State**: Use [`useState`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FDashboardChartComponent%2FpolicyComplianceChart.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FDashboardChartComponent%2FpolicyComplianceChart.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FDashboardChartComponent%2FpolicyComplianceChart.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A1%2C%22character%22%3A0%7D%5D "src/components/DashboardChartComponent/policyComplianceChart.tsx") to create a state variable [`services`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FDashboardChartComponent%2FpolicyComplianceChart.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FDashboardChartComponent%2FpolicyComplianceChart.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FDashboardChartComponent%2FpolicyComplianceChart.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A24%2C%22character%22%3A2%7D%5D "src/components/DashboardChartComponent/policyComplianceChart.tsx") containing "noncompliant" and "compliant".
2. **Define Colors**: Create a constant [`COLORS`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FDashboardChartComponent%2FpolicyComplianceChart.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FDashboardChartComponent%2FpolicyComplianceChart.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FDashboardChartComponent%2FpolicyComplianceChart.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A27%2C%22character%22%3A2%7D%5D "src/components/DashboardChartComponent/policyComplianceChart.tsx") with two color codes.
3. **Define [`applyBars`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FDashboardChartComponent%2FpolicyComplianceChart.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FDashboardChartComponent%2FpolicyComplianceChart.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FDashboardChartComponent%2FpolicyComplianceChart.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A29%2C%22character%22%3A2%7D%5D "src/components/DashboardChartComponent/policyComplianceChart.tsx") Function**:
   - Map over the [`services`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FDashboardChartComponent%2FpolicyComplianceChart.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FDashboardChartComponent%2FpolicyComplianceChart.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FDashboardChartComponent%2FpolicyComplianceChart.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A24%2C%22character%22%3A2%7D%5D "src/components/DashboardChartComponent/policyComplianceChart.tsx") set.
   - For each service, create a [`Bar`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FDashboardChartComponent%2FpolicyComplianceChart.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FDashboardChartComponent%2FpolicyComplianceChart.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FDashboardChartComponent%2FpolicyComplianceChart.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A2%2C%22character%22%3A0%7D%5D "src/components/DashboardChartComponent/policyComplianceChart.tsx") component with appropriate properties.
   - Return the array of [`Bar`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FDashboardChartComponent%2FpolicyComplianceChart.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FDashboardChartComponent%2FpolicyComplianceChart.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FDashboardChartComponent%2FpolicyComplianceChart.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A2%2C%22character%22%3A0%7D%5D "src/components/DashboardChartComponent/policyComplianceChart.tsx") components.

#### Example Usage
```tsx
import React from 'react';
import PolicyComplianceChart from './policyComplianceChart';

const App = () => (
  <div>
    <h1>Policy Compliance Dashboard</h1>
    <PolicyComplianceChart />
  </div>
);

export default App;
```

This documentation provides a detailed overview of the `policyComplianceChart.tsx` file, including its purpose, key components, and the logic behind its implementation.

sumitchohan: generate detailed documentation for current code file including all methods, fields, properties and summary of their implementation including pseudo logic

GitHub Copilot: ### Documentation for [`commentsRI.tsx`](command:_github.copilot.openRelativePath?%5B%7B%22scheme%22%3A%22file%22%2C%22authority%22%3A%22%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FRISP%2FcommentsRI.tsx%22%2C%22query%22%3A%22%22%2C%22fragment%22%3A%22%22%7D%5D "/Users/sumit/Downloads/Frontend/NAGISVPR/src/components/RISP/commentsRI.tsx")

#### Summary
This file defines a React functional component [`EditableCell`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FRISP%2FcommentsRI.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FRISP%2FcommentsRI.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FRISP%2FcommentsRI.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A6%2C%22character%22%3A0%7D%5D "src/components/RISP/commentsRI.tsx") that allows users to edit comments. It includes state management for editing mode, character count, loading state, and error handling. The component also handles saving and canceling edits, with integration to an API for posting comments.

#### Component: [`EditableCell`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FRISP%2FcommentsRI.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FRISP%2FcommentsRI.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FRISP%2FcommentsRI.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A6%2C%22character%22%3A0%7D%5D "src/components/RISP/commentsRI.tsx")

##### Props
- `vm_Id`: Identifier for the virtual machine.
- `userName`: Name of the user.
- `onCommentUpdate`: Callback function to update the comment.
- `subscription_id`: Identifier for the subscription.
- [`usercomment`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FRISP%2FcommentsRI.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FRISP%2FcommentsRI.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FRISP%2FcommentsRI.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A6%2C%22character%22%3A22%7D%5D "src/components/RISP/commentsRI.tsx"): Initial comment provided by the user.

##### State Variables
- [`isEditing`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FRISP%2FcommentsRI.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FRISP%2FcommentsRI.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FRISP%2FcommentsRI.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A13%2C%22character%22%3A2%7D%5D "src/components/RISP/commentsRI.tsx"): Boolean state to track if the cell is in editing mode.
- [`comments`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FRISP%2FcommentsRI.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FRISP%2FcommentsRI.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FRISP%2FcommentsRI.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A14%2C%22character%22%3A2%7D%5D "src/components/RISP/commentsRI.tsx"): State to store the current comment text.
- [`initialComments`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FRISP%2FcommentsRI.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FRISP%2FcommentsRI.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FRISP%2FcommentsRI.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A15%2C%22character%22%3A2%7D%5D "src/components/RISP/commentsRI.tsx"): State to store the initial comment text before editing.
- [`charCount`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FRISP%2FcommentsRI.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FRISP%2FcommentsRI.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FRISP%2FcommentsRI.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A16%2C%22character%22%3A2%7D%5D "src/components/RISP/commentsRI.tsx"): State to store the character count of the comment.
- [`isLoading`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FRISP%2FcommentsRI.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FRISP%2FcommentsRI.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FRISP%2FcommentsRI.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A19%2C%22character%22%3A2%7D%5D "src/components/RISP/commentsRI.tsx"): Boolean state to track if the save operation is in progress.
- [`error`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FRISP%2FcommentsRI.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FRISP%2FcommentsRI.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FRISP%2FcommentsRI.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A20%2C%22character%22%3A2%7D%5D "src/components/RISP/commentsRI.tsx"): State to store any error that occurs during the save operation.

##### Methods

###### [`useEffect`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FRISP%2FcommentsRI.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FRISP%2FcommentsRI.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FRISP%2FcommentsRI.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A0%2C%22character%22%3A0%7D%5D "src/components/RISP/commentsRI.tsx")
- **Purpose**: To reset the comment states whenever [`usercomment`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FRISP%2FcommentsRI.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FRISP%2FcommentsRI.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FRISP%2FcommentsRI.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A6%2C%22character%22%3A22%7D%5D "src/components/RISP/commentsRI.tsx") prop changes.
- **Pseudo Logic**:
  1. Set [`comments`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FRISP%2FcommentsRI.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FRISP%2FcommentsRI.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FRISP%2FcommentsRI.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A14%2C%22character%22%3A2%7D%5D "src/components/RISP/commentsRI.tsx") to [`usercomment`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FRISP%2FcommentsRI.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FRISP%2FcommentsRI.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FRISP%2FcommentsRI.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A6%2C%22character%22%3A22%7D%5D "src/components/RISP/commentsRI.tsx") or an empty string.
  2. Set [`initialComments`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FRISP%2FcommentsRI.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FRISP%2FcommentsRI.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FRISP%2FcommentsRI.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A15%2C%22character%22%3A2%7D%5D "src/components/RISP/commentsRI.tsx") to [`usercomment`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FRISP%2FcommentsRI.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FRISP%2FcommentsRI.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FRISP%2FcommentsRI.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A6%2C%22character%22%3A22%7D%5D "src/components/RISP/commentsRI.tsx") or an empty string.
  3. Set [`charCount`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FRISP%2FcommentsRI.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FRISP%2FcommentsRI.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FRISP%2FcommentsRI.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A16%2C%22character%22%3A2%7D%5D "src/components/RISP/commentsRI.tsx") to the length of [`usercomment`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FRISP%2FcommentsRI.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FRISP%2FcommentsRI.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FRISP%2FcommentsRI.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A6%2C%22character%22%3A22%7D%5D "src/components/RISP/commentsRI.tsx") or 0.

###### [`handleEditIconClick`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FRISP%2FcommentsRI.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FRISP%2FcommentsRI.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FRISP%2FcommentsRI.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A28%2C%22character%22%3A2%7D%5D "src/components/RISP/commentsRI.tsx")
- **Purpose**: To enable editing mode and set initial values for editing.
- **Pseudo Logic**:
  1. Set [`initialComments`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FRISP%2FcommentsRI.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FRISP%2FcommentsRI.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FRISP%2FcommentsRI.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A15%2C%22character%22%3A2%7D%5D "src/components/RISP/commentsRI.tsx") to the current [`comments`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FRISP%2FcommentsRI.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FRISP%2FcommentsRI.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FRISP%2FcommentsRI.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A14%2C%22character%22%3A2%7D%5D "src/components/RISP/commentsRI.tsx").
  2. Set [`charCount`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FRISP%2FcommentsRI.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FRISP%2FcommentsRI.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FRISP%2FcommentsRI.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A16%2C%22character%22%3A2%7D%5D "src/components/RISP/commentsRI.tsx") to the length of [`initialComments`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FRISP%2FcommentsRI.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FRISP%2FcommentsRI.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FRISP%2FcommentsRI.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A15%2C%22character%22%3A2%7D%5D "src/components/RISP/commentsRI.tsx").
  3. Set [`isEditing`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FRISP%2FcommentsRI.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FRISP%2FcommentsRI.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FRISP%2FcommentsRI.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A13%2C%22character%22%3A2%7D%5D "src/components/RISP/commentsRI.tsx") to `true`.

###### [`handleOnClose`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FRISP%2FcommentsRI.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FRISP%2FcommentsRI.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FRISP%2FcommentsRI.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A34%2C%22character%22%3A2%7D%5D "src/components/RISP/commentsRI.tsx")
- **Purpose**: To cancel editing and revert to the initial comment.
- **Pseudo Logic**:
  1. Set [`comments`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FRISP%2FcommentsRI.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FRISP%2FcommentsRI.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FRISP%2FcommentsRI.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A14%2C%22character%22%3A2%7D%5D "src/components/RISP/commentsRI.tsx") to [`initialComments`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FRISP%2FcommentsRI.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FRISP%2FcommentsRI.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FRISP%2FcommentsRI.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A15%2C%22character%22%3A2%7D%5D "src/components/RISP/commentsRI.tsx").
  2. Set [`isEditing`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FRISP%2FcommentsRI.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FRISP%2FcommentsRI.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FRISP%2FcommentsRI.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A13%2C%22character%22%3A2%7D%5D "src/components/RISP/commentsRI.tsx") to `false`.

###### [`handleSave`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FRISP%2FcommentsRI.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FRISP%2FcommentsRI.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FRISP%2FcommentsRI.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A39%2C%22character%22%3A2%7D%5D "src/components/RISP/commentsRI.tsx")
- **Purpose**: To save the edited comment.
- **Pseudo Logic**:
  1. Check if [`comments`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FRISP%2FcommentsRI.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FRISP%2FcommentsRI.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FRISP%2FcommentsRI.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A14%2C%22character%22%3A2%7D%5D "src/components/RISP/commentsRI.tsx") is empty after trimming whitespace.
  2. If not empty, proceed with saving logic (not fully shown in the provided code).

##### Example Usage
```tsx
<EditableCell
  vm_Id="123"
  userName="John Doe"
  onCommentUpdate={updateCommentHandler}
  subscription_id="sub-456"
  usercomment="Initial comment"
/>
```

This documentation provides an overview of the `EditableCell` component, its props, state variables, and methods, along with their purposes and pseudo logic.

sumitchohan: generate detailed documentation for current code file including all methods, fields, properties and summary of their implementation including pseudo logic

GitHub Copilot: To generate detailed documentation for the provided code file [`commitedUsageDiscounts.tsx`](command:_github.copilot.openRelativePath?%5B%7B%22scheme%22%3A%22file%22%2C%22authority%22%3A%22%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FRISP%2FcommitedUsageDiscounts.tsx%22%2C%22query%22%3A%22%22%2C%22fragment%22%3A%22%22%7D%5D "/Users/sumit/Downloads/Frontend/NAGISVPR/src/components/RISP/commitedUsageDiscounts.tsx"), we will follow these steps:

1. **File Overview**: Provide a brief summary of the file's purpose.
2. **Imports**: List and describe each import and its purpose.
3. **Components and Functions**: Identify and describe each component and function used in the file.

### File Overview
[`commitedUsageDiscounts.tsx`](command:_github.copilot.openRelativePath?%5B%7B%22scheme%22%3A%22file%22%2C%22authority%22%3A%22%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FRISP%2FcommitedUsageDiscounts.tsx%22%2C%22query%22%3A%22%22%2C%22fragment%22%3A%22%22%7D%5D "/Users/sumit/Downloads/Frontend/NAGISVPR/src/components/RISP/commitedUsageDiscounts.tsx") is a React component file that appears to handle the display and management of committed usage discounts. It imports various utilities, components, and APIs to fetch data, render UI elements, and manage state.

### Imports
1. **React Hooks**:
   - [`useEffect`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FRISP%2FcommitedUsageDiscounts.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FRISP%2FcommitedUsageDiscounts.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FRISP%2FcommitedUsageDiscounts.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A0%2C%22character%22%3A0%7D%5D "src/components/RISP/commitedUsageDiscounts.tsx"), [`useRef`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FRISP%2FcommitedUsageDiscounts.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FRISP%2FcommitedUsageDiscounts.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FRISP%2FcommitedUsageDiscounts.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A0%2C%22character%22%3A0%7D%5D "src/components/RISP/commitedUsageDiscounts.tsx"), [`useState`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FRISP%2FcommitedUsageDiscounts.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FRISP%2FcommitedUsageDiscounts.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FRISP%2FcommitedUsageDiscounts.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A0%2C%22character%22%3A0%7D%5D "src/components/RISP/commitedUsageDiscounts.tsx"): Standard React hooks for managing side effects, references, and component state.

2. **Custom Components**:
   - [`CustomSelect`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FcustomSelect.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A13%2C%22character%22%3A0%7D%5D "src/components/customSelect.tsx"), [`CustomSearchSelect`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FcustomSearchSelect.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A13%2C%22character%22%3A0%7D%5D "src/components/customSearchSelect.tsx"), [`SearchBar`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FsearchBar.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A8%2C%22character%22%3A0%7D%5D "src/components/searchBar.tsx"), [`PolicyListRI`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FRISP%2FpolicyListRI.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A27%2C%22character%22%3A0%7D%5D "src/components/RISP/policyListRI.tsx"), [`ScheduleRun`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FscheduleRun.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A32%2C%22character%22%3A0%7D%5D "src/components/scheduleRun.tsx"), [`ProposedPurchaseView`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FRISP%2FproposedPurchase.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A11%2C%22character%22%3A0%7D%5D "src/components/RISP/proposedPurchase.tsx"), [`CudFilter`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FRISP%2FcudFilter.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A42%2C%22character%22%3A0%7D%5D "src/components/RISP/cudFilter.tsx"), [`Loader`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2Floader.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A7%2C%22character%22%3A0%7D%5D "src/components/loader.tsx"): Custom components likely used for rendering specific UI elements and functionalities.

3. **APIs**:
   - [`getDateList`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FclientAPI.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A37%2C%22character%22%3A0%7D%5D "src/apis/clientAPI.tsx"), [`getAllAccounts`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FaccountAPI.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A51%2C%22character%22%3A0%7D%5D "src/apis/accountAPI.tsx"), [`getAllTenants`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FgetTenentAPI.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FgetTenentAPI.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FgetTenentAPI.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A22%2C%22character%22%3A0%7D%5D "src/apis/getTenentAPI.tsx"), [`getAllCommittedUsage`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FgetTenentAPI.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FgetTenentAPI.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FgetTenentAPI.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A35%2C%22character%22%3A0%7D%5D "src/apis/getTenentAPI.tsx"), [`getCommittedUsage`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FgetCommitedUsage.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A3%2C%22character%22%3A0%7D%5D "src/apis/getCommitedUsage.tsx"): Functions to fetch data from various APIs.

4. **Utilities and Constants**:
   - [`Account`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Futils%2Finterfaces.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A81%2C%22character%22%3A0%7D%5D "src/utils/interfaces.tsx"), [`CommittedUsage`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Futils%2Finterfaces.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A99%2C%22character%22%3A0%7D%5D "src/utils/interfaces.tsx"), [`Policy`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Futils%2Finterfaces.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A9%2C%22character%22%3A0%7D%5D "src/utils/interfaces.tsx"), [`PolicyCompStatus`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Futils%2Finterfaces.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A30%2C%22character%22%3A0%7D%5D "src/utils/interfaces.tsx"), [`PolicySummaryGovernance`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Futils%2Finterfaces.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A37%2C%22character%22%3A0%7D%5D "src/utils/interfaces.tsx"): TypeScript interfaces for type definitions.
   - [`CLOUD_PROVIDERS`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Futils%2Fconstants.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A52%2C%22character%22%3A0%7D%5D "src/utils/constants.tsx"), [`FILTER_OPTIONS`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Futils%2Fconstants.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A97%2C%22character%22%3A0%7D%5D "src/utils/constants.tsx"), [`FILTER_OPTION_VALUES`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Futils%2Fconstants.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A106%2C%22character%22%3A0%7D%5D "src/utils/constants.tsx"), [`PILLARS`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Futils%2Fconstants.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A54%2C%22character%22%3A0%7D%5D "src/utils/constants.tsx"), [`POLICY_SHOW_COLS`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Futils%2Fconstants.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A72%2C%22character%22%3A0%7D%5D "src/utils/constants.tsx"), [`SORT_OPTIONS`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Futils%2Fconstants.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A83%2C%22character%22%3A0%7D%5D "src/utils/constants.tsx"): Constants used for filtering, sorting, and displaying data.

5. **Icons**:
   - [`FontAwesomeIcon`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FRISP%2FcommitedUsageDiscounts.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FRISP%2FcommitedUsageDiscounts.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FRISP%2FcommitedUsageDiscounts.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A21%2C%22character%22%3A0%7D%5D "src/components/RISP/commitedUsageDiscounts.tsx"), [`faArrowRotateRight`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FRISP%2FcommitedUsageDiscounts.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FRISP%2FcommitedUsageDiscounts.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FRISP%2FcommitedUsageDiscounts.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A22%2C%22character%22%3A0%7D%5D "src/components/RISP/commitedUsageDiscounts.tsx"), [`faDownload`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FRISP%2FcommitedUsageDiscounts.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FRISP%2FcommitedUsageDiscounts.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FRISP%2FcommitedUsageDiscounts.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A22%2C%22character%22%3A0%7D%5D "src/components/RISP/commitedUsageDiscounts.tsx"), [`faEye`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FRISP%2FcommitedUsageDiscounts.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FRISP%2FcommitedUsageDiscounts.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FRISP%2FcommitedUsageDiscounts.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A22%2C%22character%22%3A0%7D%5D "src/components/RISP/commitedUsageDiscounts.tsx"), [`RemoveRedEyeIcon`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FRISP%2FcommitedUsageDiscounts.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FRISP%2FcommitedUsageDiscounts.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FRISP%2FcommitedUsageDiscounts.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A27%2C%22character%22%3A0%7D%5D "src/components/RISP/commitedUsageDiscounts.tsx"), [`DownloadIcon`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FRISP%2FcommitedUsageDiscounts.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FRISP%2FcommitedUsageDiscounts.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FRISP%2FcommitedUsageDiscounts.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A28%2C%22character%22%3A0%7D%5D "src/components/RISP/commitedUsageDiscounts.tsx"), [`FilterAltIcon`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FRISP%2FcommitedUsageDiscounts.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FRISP%2FcommitedUsageDiscounts.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FRISP%2FcommitedUsageDiscounts.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A29%2C%22character%22%3A0%7D%5D "src/components/RISP/commitedUsageDiscounts.tsx"), [`ArrowCircleUpIcon`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FRISP%2FcommitedUsageDiscounts.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FRISP%2FcommitedUsageDiscounts.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FRISP%2FcommitedUsageDiscounts.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A32%2C%22character%22%3A0%7D%5D "src/components/RISP/commitedUsageDiscounts.tsx"): Icons used for UI elements.

6. **CSV Export**:
   - [`CSVLink`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FRISP%2FcommitedUsageDiscounts.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FRISP%2FcommitedUsageDiscounts.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FRISP%2FcommitedUsageDiscounts.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A39%2C%22character%22%3A0%7D%5D "src/components/RISP/commitedUsageDiscounts.tsx"): Component for exporting data to CSV.

7. **Date Utilities**:
   - [`format`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FRISP%2FcommitedUsageDiscounts.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FRISP%2FcommitedUsageDiscounts.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FRISP%2FcommitedUsageDiscounts.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A40%2C%22character%22%3A0%7D%5D "src/components/RISP/commitedUsageDiscounts.tsx"), [`toZonedTime`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FRISP%2FcommitedUsageDiscounts.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FRISP%2FcommitedUsageDiscounts.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FRISP%2FcommitedUsageDiscounts.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A40%2C%22character%22%3A0%7D%5D "src/components/RISP/commitedUsageDiscounts.tsx"): Functions for date formatting and timezone conversion.

### Components and Functions

Since the provided code snippet does not include the actual implementation of the component, we will outline the expected structure and logic based on the imports.

#### Pseudo Logic
1. **State Management**:
   - Use [`useState`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FRISP%2FcommitedUsageDiscounts.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FRISP%2FcommitedUsageDiscounts.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FRISP%2FcommitedUsageDiscounts.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A0%2C%22character%22%3A0%7D%5D "src/components/RISP/commitedUsageDiscounts.tsx") to manage component state such as selected filters, accounts, committed usage data, etc.

2. **Data Fetching**:
   - Use [`useEffect`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FRISP%2FcommitedUsageDiscounts.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FRISP%2FcommitedUsageDiscounts.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FRISP%2FcommitedUsageDiscounts.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A0%2C%22character%22%3A0%7D%5D "src/components/RISP/commitedUsageDiscounts.tsx") to fetch data from APIs ([`getDateList`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FclientAPI.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A37%2C%22character%22%3A0%7D%5D "src/apis/clientAPI.tsx"), [`getAllAccounts`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FaccountAPI.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A51%2C%22character%22%3A0%7D%5D "src/apis/accountAPI.tsx"), [`getAllTenants`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FgetTenentAPI.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FgetTenentAPI.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FgetTenentAPI.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A22%2C%22character%22%3A0%7D%5D "src/apis/getTenentAPI.tsx"), [`getAllCommittedUsage`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FgetTenentAPI.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FgetTenentAPI.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FgetTenentAPI.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A35%2C%22character%22%3A0%7D%5D "src/apis/getTenentAPI.tsx"), [`getCommittedUsage`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FgetCommitedUsage.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A3%2C%22character%22%3A0%7D%5D "src/apis/getCommitedUsage.tsx")) when the component mounts or when dependencies change.

3. **Rendering UI**:
   - Render various custom components ([`CustomSelect`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FcustomSelect.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A13%2C%22character%22%3A0%7D%5D "src/components/customSelect.tsx"), [`CustomSearchSelect`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FcustomSearchSelect.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A13%2C%22character%22%3A0%7D%5D "src/components/customSearchSelect.tsx"), [`SearchBar`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FsearchBar.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A8%2C%22character%22%3A0%7D%5D "src/components/searchBar.tsx"), [`PolicyListRI`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FRISP%2FpolicyListRI.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A27%2C%22character%22%3A0%7D%5D "src/components/RISP/policyListRI.tsx"), [`ScheduleRun`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FscheduleRun.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A32%2C%22character%22%3A0%7D%5D "src/components/scheduleRun.tsx"), [`ProposedPurchaseView`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FRISP%2FproposedPurchase.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A11%2C%22character%22%3A0%7D%5D "src/components/RISP/proposedPurchase.tsx"), [`CudFilter`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FRISP%2FcudFilter.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A42%2C%22character%22%3A0%7D%5D "src/components/RISP/cudFilter.tsx"), [`Loader`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2Floader.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A7%2C%22character%22%3A0%7D%5D "src/components/loader.tsx")) to display data and provide user interaction options.
   - Use icons ([`FontAwesomeIcon`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FRISP%2FcommitedUsageDiscounts.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FRISP%2FcommitedUsageDiscounts.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FRISP%2FcommitedUsageDiscounts.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A21%2C%22character%22%3A0%7D%5D "src/components/RISP/commitedUsageDiscounts.tsx"), [`RemoveRedEyeIcon`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FRISP%2FcommitedUsageDiscounts.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FRISP%2FcommitedUsageDiscounts.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FRISP%2FcommitedUsageDiscounts.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A27%2C%22character%22%3A0%7D%5D "src/components/RISP/commitedUsageDiscounts.tsx"), [`DownloadIcon`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FRISP%2FcommitedUsageDiscounts.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FRISP%2FcommitedUsageDiscounts.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FRISP%2FcommitedUsageDiscounts.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A28%2C%22character%22%3A0%7D%5D "src/components/RISP/commitedUsageDiscounts.tsx"), [`FilterAltIcon`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FRISP%2FcommitedUsageDiscounts.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FRISP%2FcommitedUsageDiscounts.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FRISP%2FcommitedUsageDiscounts.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A29%2C%22character%22%3A0%7D%5D "src/components/RISP/commitedUsageDiscounts.tsx"), [`ArrowCircleUpIcon`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FRISP%2FcommitedUsageDiscounts.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FRISP%2FcommitedUsageDiscounts.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FRISP%2FcommitedUsageDiscounts.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A32%2C%22character%22%3A0%7D%5D "src/components/RISP/commitedUsageDiscounts.tsx")) to enhance the UI.

4. **Event Handling**:
   - Handle user interactions such as selecting filters, searching, and exporting data to CSV using event handlers.

### Example Documentation

```markdown
# commitedUsageDiscounts.tsx

## Overview
This file defines a React component responsible for managing and displaying committed usage discounts. It integrates various custom components, APIs, and utilities to fetch data, render UI elements, and handle user interactions.

## Imports

### React Hooks
- `useEffect`: Manages side effects in the component.
- `useRef`: Creates a mutable object that persists across renders.
- `useState`: Manages component state.

### Custom Components
- `CustomSelect`: A dropdown select component.
- `CustomSearchSelect`: A searchable dropdown select component.
- `SearchBar`: A component for searching data.
- `PolicyListRI`: Displays a list of policies.
- `ScheduleRun`: Manages scheduling of runs.
- `ProposedPurchaseView`: Displays proposed purchases.
- `CudFilter`: Filters committed usage data.
- `Loader`: Displays a loading spinner.

### APIs
- `getDateList`: Fetches a list of dates.
- `getAllAccounts`: Fetches all accounts.
- `getAllTenants`: Fetches all tenants.
- `getAllCommittedUsage`: Fetches all committed usage data.
- `getCommittedUsage`: Fetches specific committed usage data.

### Utilities and Constants
- `Account`, `CommittedUsage`, `Policy`, `PolicyCompStatus`, `PolicySummaryGovernance`: TypeScript interfaces for type definitions.
- `CLOUD_PROVIDERS`, `FILTER_OPTIONS`, `FILTER_OPTION_VALUES`, `PILLARS`, `POLICY_SHOW_COLS`, `SORT_OPTIONS`: Constants for filtering, sorting, and displaying data.

### Icons
- `FontAwesomeIcon`: Wrapper for FontAwesome icons.
- `faArrowRotateRight`, `faDownload`, `faEye`: FontAwesome icons.
- `RemoveRedEyeIcon`, `DownloadIcon`, `FilterAltIcon`, `ArrowCircleUpIcon`: Material UI icons.

### CSV Export
- `CSVLink`: Component for exporting data to CSV.

### Date Utilities
- `format`: Formats dates.
- `toZonedTime`: Converts dates to a specific timezone.

## Component Logic

### State Management
- Use `useState` to manage state variables such as filters, accounts, and usage data.

### Data Fetching
- Use `useEffect` to fetch data from APIs when the component mounts or dependencies change.

### Rendering UI
- Render custom components to display data and provide user interaction options.
- Use icons to enhance the UI.

### Event Handling
- Handle user interactions such as selecting filters, searching, and exporting data to CSV.
```

This documentation provides a comprehensive overview of the file, its imports, and the expected logic within the component.

sumitchohan: generate detailed documentation for current code file including all methods, fields, properties and summary of their implementation including pseudo logic

GitHub Copilot: ### File: [`cudFilter.tsx`](command:_github.copilot.openRelativePath?%5B%7B%22scheme%22%3A%22file%22%2C%22authority%22%3A%22%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FRISP%2FcudFilter.tsx%22%2C%22query%22%3A%22%22%2C%22fragment%22%3A%22%22%7D%5D "/Users/sumit/Downloads/Frontend/NAGISVPR/src/components/RISP/cudFilter.tsx")

This file defines a React component that provides a filter interface for a policy tray. It imports various libraries and components, including FontAwesome icons, Material-UI components, and a date picker. The component is designed to handle multiple filter criteria such as location, VM size, tenure, subscription, and date range.

#### Imports
- **FontAwesomeIcon**: Used for rendering icons.
- **PolicyTray.css**: CSS file for styling the component.
- **Material-UI Components**: Various components from Material-UI for building the UI.
- **DatePicker**: A date picker component for selecting dates.
- **getFiltersRI**: An API function to fetch filter data.

#### Interface: [`cudFilterProps`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FRISP%2FcudFilter.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FRISP%2FcudFilter.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FRISP%2FcudFilter.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A20%2C%22character%22%3A0%7D%5D "src/components/RISP/cudFilter.tsx")
Defines the properties that the `cudFilter` component expects to receive.

- **isOpen**: `boolean` - Indicates if the filter tray is open.
- **onClose**: `() => void` - Function to close the filter tray.
- **savedLocation**: `string[]` - Array of saved locations.
- **setSavedLocation**: `React.Dispatch<React.SetStateAction<string[]>>` - Function to update saved locations.
- **savedVmSize**: `string[]` - Array of saved VM sizes.
- **setSavedVmSize**: `React.Dispatch<React.SetStateAction<string[]>>` - Function to update saved VM sizes.
- **savedTenure**: `string[]` - Array of saved tenures.
- **setSavedTenure**: `React.Dispatch<React.SetStateAction<string[]>>` - Function to update saved tenures.
- **savedSubscription**: `string[]` - Array of saved subscriptions.
- **setSavedSubscription**: `React.Dispatch<React.SetStateAction<string[]>>` - Function to update saved subscriptions.
- **savedMinValue**: `number` - Minimum value for a range filter.
- **setSavedMinValue**: `React.Dispatch<React.SetStateAction<number>>` - Function to update the minimum value.
- **savedMaxValue**: `number` - Maximum value for a range filter.
- **setSavedMaxValue**: `React.Dispatch<React.SetStateAction<number>>` - Function to update the maximum value.
- **savedStartDate**: [`Date | null`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22external%22%3A%22file%3A%2F%2F%2FApplications%2FVisual%2520Studio%2520Code.app%2FContents%2FResources%2Fapp%2Fextensions%2Fnode_modules%2Ftypescript%2Flib%2Flib.es2015.symbol.wellknown.d.ts%22%2C%22path%22%3A%22%2FApplications%2FVisual%20Studio%20Code.app%2FContents%2FResources%2Fapp%2Fextensions%2Fnode_modules%2Ftypescript%2Flib%2Flib.es2015.symbol.wellknown.d.ts%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A111%2C%22character%22%3A0%7D%5D "../../../../../Applications/Visual Studio Code.app/Contents/Resources/app/extensions/node_modules/typescript/lib/lib.es2015.symbol.wellknown.d.ts") - Start date for a date range filter.
- **setSavedStartDate**: `React.Dispatch<React.SetStateAction<Date | null>>` - Function to update the start date.
- **savedEndDate**: [`Date | null`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22external%22%3A%22file%3A%2F%2F%2FApplications%2FVisual%2520Studio%2520Code.app%2FContents%2FResources%2Fapp%2Fextensions%2Fnode_modules%2Ftypescript%2Flib%2Flib.es2015.symbol.wellknown.d.ts%22%2C%22path%22%3A%22%2FApplications%2FVisual%20Studio%20Code.app%2FContents%2FResources%2Fapp%2Fextensions%2Fnode_modules%2Ftypescript%2Flib%2Flib.es2015.symbol.wellknown.d.ts%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A111%2C%22character%22%3A0%7D%5D "../../../../../Applications/Visual Studio Code.app/Contents/Resources/app/extensions/node_modules/typescript/lib/lib.es2015.symbol.wellknown.d.ts") - End date for a date range filter.
- **setSavedEndDate**: `React.Dispatch<React.SetStateAction<Date | null>>` - Function to update the end date.
- **tenantId**: `string` - Tenant ID for the current user.

### Pseudo Logic

1. **Component Initialization**:
   - Import necessary libraries and components.
   - Define the [`cudFilterProps`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FRISP%2FcudFilter.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FRISP%2FcudFilter.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FRISP%2FcudFilter.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A20%2C%22character%22%3A0%7D%5D "src/components/RISP/cudFilter.tsx") interface to specify the expected props.

2. **Component Definition**:
   - Define the `cudFilter` component.
   - Use state hooks to manage the state of various filters.
   - Use effect hooks to fetch initial filter data when the component mounts.

3. **Render Method**:
   - Render the filter tray UI.
   - Include form controls for each filter criterion (location, VM size, tenure, subscription, value range, date range).
   - Use Material-UI components for consistent styling and behavior.
   - Use FontAwesome icons for visual indicators.
   - Include a date picker for selecting date ranges.

4. **Event Handlers**:
   - Define event handlers to update the state when the user interacts with the filters.
   - Ensure that changes in the filter criteria are reflected in the parent component via the provided setter functions.

### Example Code Block

```tsx
import { FontAwesomeIcon } from "@fortawesome/react-fontawesome";
import "../../components-css/PolicyTray.css";
import { faCircle, faCircleXmark } from "@fortawesome/free-solid-svg-icons";
import { useEffect, useRef, useState } from "react";
import {
  FormGroup,
  FormControlLabel,
  Checkbox,
  FormControl,
  Slider,
  Typography,
  Tooltip,
} from "@mui/material";
import DatePicker from "react-datepicker";
import "react-datepicker/dist/react-datepicker.css";
import CalendarMonthIcon from "@mui/icons-material/CalendarMonth";
import InputAdornment from "@mui/material/InputAdornment";
import TextField from "@mui/material/TextField";
import { getFiltersRI } from "../../apis/getRISPFilters";

interface cudFilterProps {
  isOpen: boolean;
  onClose: () => void;
  savedLocation: string[];
  setSavedLocation: React.Dispatch<React.SetStateAction<string[]>>;
  savedVmSize: string[];
  setSavedVmSize: React.Dispatch<React.SetStateAction<string[]>>;
  savedTenure: string[];
  setSavedTenure: React.Dispatch<React.SetStateAction<string[]>>;
  savedSubscription: string[];
  setSavedSubscription: React.Dispatch<React.SetStateAction<string[]>>;
  savedMinValue: number;
  setSavedMinValue: React.Dispatch<React.SetStateAction<number>>;
  savedMaxValue: number;
  setSavedMaxValue: React.Dispatch<React.SetStateAction<number>>;
  savedStartDate: Date | null;
  setSavedStartDate: React.Dispatch<React.SetStateAction<Date | null>>;
  savedEndDate: Date | null;
  setSavedEndDate: React.Dispatch<React.SetStateAction<Date | null>>;
  tenantId: string;
}

// Component definition and implementation would follow here.
```

This documentation provides a detailed overview of the `cudFilter.tsx` file, including its imports, interface definition, and pseudo logic for the component's implementation.

sumitchohan: generate detailed documentation for current code file including all methods, fields, properties and summary of their implementation including pseudo logic


GitHub Copilot: ### Documentation for [`customTenantSearchSelect.tsx`](command:_github.copilot.openRelativePath?%5B%7B%22scheme%22%3A%22file%22%2C%22authority%22%3A%22%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FRISP%2FcustomTenantSearchSelect.tsx%22%2C%22query%22%3A%22%22%2C%22fragment%22%3A%22%22%7D%5D "/Users/sumit/Downloads/Frontend/NAGISVPR/src/components/RISP/customTenantSearchSelect.tsx")

#### Summary
This file defines a React functional component named `customTenantSearchSelect`. The component is a custom dropdown selector that allows users to select an option from a list of key-value pairs. It includes features such as dropdown toggling, option selection, and search functionality.

#### Props
- **optionList**: `Record<string, string>` - A list of options in key-value pair format.
- **selectedOption**: `string` - The currently selected option value.
- **selectedCSS**: `string` - CSS class for the selected option.
- **optionCSS**: `string` - CSS class for the options in the dropdown.
- **arrowCSS**: `string` - CSS class for the dropdown arrow.
- **onChange**: `(value: string) => void` - Callback function triggered when an option is selected.
- **selectedKeyValue**: `{ key: string; value: string } | null` - The currently selected key-value pair.
- **setSelectedKeyValue**: `React.Dispatch<React.SetStateAction<{ key: string; value: string } | null>>` - Function to update the selected key-value pair.

#### State
- **isDropdownOpen**: `boolean` - State to track whether the dropdown is open or closed.
- **selectedValue**: `string` - State to track the currently selected option value.
- **searchTerm**: `string` - State to track the search term input by the user.

#### Methods

1. **toggleDropdown**
   - **Description**: Toggles the dropdown open or closed.
   - **Pseudo Logic**:
     - If `isDropdownOpen` is `true`, set it to `false`.
     - If `isDropdownOpen` is `false`, set it to `true`.

2. **handleOptionClick**
   - **Parameters**: `key: string`, `value: string`
   - **Description**: Handles the selection of an option from the dropdown.
   - **Pseudo Logic**:
     - Set `selectedValue` to the selected `value`.
     - Update `selectedKeyValue` with the selected `key` and `value`.
     - Close the dropdown by setting `isDropdownOpen` to `false`.
     - Trigger the `onChange` callback with the selected `value`.

3. **useEffect**
   - **Description**: React hook that runs side effects.
   - **Pseudo Logic**:
     - On component mount or when `selectedOption` changes:
       - Set `selectedValue` to `props.selectedOption`.
       - Find the entry in `optionList` where the value matches `props.selectedOption`.
       - If found, update `selectedKeyValue` with the corresponding key-value pair.

#### Component Implementation
```tsx
import { useEffect, useState } from "react";

interface customTenantSearchSelectProps {
  optionList: Record<string, string>;
  selectedOption: string;
  selectedCSS: string;
  optionCSS: string;
  arrowCSS: string;
  onChange: (value: string) => void;
  selectedKeyValue: { key: string; value: string } | null;
  setSelectedKeyValue: React.Dispatch<React.SetStateAction<{ key: string; value: string } | null>>;
}

const customTenantSearchSelect: React.FC<customTenantSearchSelectProps> = (props) => {
  const [isDropdownOpen, setDropdownOpen] = useState<boolean>(false);
  const [selectedValue, setSelectedValue] = useState<string>(props.selectedOption);
  const [searchTerm, setSearchTerm] = useState<string>("");

  const toggleDropdown = () => {
    setDropdownOpen(!isDropdownOpen);
  };

  const handleOptionClick = (key: string, value: string) => {
    setSelectedValue(value);
    props.setSelectedKeyValue({ key, value });
    setDropdownOpen(false);
    props.onChange(value);
  };

  useEffect(() => {
    setSelectedValue(props.selectedOption);
    const selectedEntry = Object.entries(props.optionList).find(
      ([_, value]) => value === props.selectedOption
    );
    if (selectedEntry) {
      props.setSelectedKeyValue({ key: selectedEntry[0], value: selectedEntry[1] });
    }
  }, [props.selectedOption, props.optionList, props.setSelectedKeyValue]);

  return (
    // JSX for rendering the component goes here
  );
};

export default customTenantSearchSelect;
```

This documentation provides a detailed overview of the component's props, state, methods, and their respective logic.

sumitchohan: generate detailed documentation for current code file including all methods, fields, properties and summary of their implementation including pseudo logic



GitHub Copilot: ### Documentation for [`dropdownTenureRI.tsx`](command:_github.copilot.openRelativePath?%5B%7B%22scheme%22%3A%22file%22%2C%22authority%22%3A%22%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FRISP%2FdropdownTenureRI.tsx%22%2C%22query%22%3A%22%22%2C%22fragment%22%3A%22%22%7D%5D "/Users/sumit/Downloads/Frontend/NAGISVPR/src/components/RISP/dropdownTenureRI.tsx")

#### Summary
This file defines a React functional component named [`Dropdown`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FRISP%2FdropdownTenureRI.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FRISP%2FdropdownTenureRI.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FRISP%2FdropdownTenureRI.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A15%2C%22character%22%3A0%7D%5D "src/components/RISP/dropdownTenureRI.tsx"). The component is a custom dropdown selector that allows users to select an option from a list. It includes features such as dropdown toggling and option selection. Additionally, it interacts with an API to set tenure and displays notifications using `react-hot-toast`.

#### Props
- **options**: `string[]` - An array of options to be displayed in the dropdown.
- **value**: `string` (optional) - The currently selected option value.
- **onChange**: `(newValue: string) => void` - Callback function triggered when an option is selected.
- **vm_id**: `string` (optional) - Virtual machine ID.
- **vm_name**: `string` (optional) - Virtual machine name.
- **subscription_id**: `string` (optional) - Subscription ID.
- **location**: `string` (optional) - Location.

#### State
- **isOpen**: `boolean` - State to track whether the dropdown is open or closed.
- **selectedValue**: `string` - State to track the currently selected option value.
- **dropdownPosition**: `"bottom" | "top"` - State to track the position of the dropdown (either "bottom" or "top").

#### Refs
- **containerRef**: [`React.RefObject<HTMLDivElement>`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FRISP%2FdropdownTenureRI.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FRISP%2FdropdownTenureRI.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FRISP%2FdropdownTenureRI.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A0%2C%22character%22%3A0%7D%5D "src/components/RISP/dropdownTenureRI.tsx") - Reference to the dropdown container element.

#### Methods

1. **toggleDropdown**
   - **Description**: Toggles the dropdown open or closed.
   - **Pseudo Logic**:
     - If [`isOpen`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FRISP%2FdropdownTenureRI.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FRISP%2FdropdownTenureRI.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FRISP%2FdropdownTenureRI.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A24%2C%22character%22%3A2%7D%5D "src/components/RISP/dropdownTenureRI.tsx") is `true`, set it to `false`.
     - If [`isOpen`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FRISP%2FdropdownTenureRI.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FRISP%2FdropdownTenureRI.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FRISP%2FdropdownTenureRI.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A24%2C%22character%22%3A2%7D%5D "src/components/RISP/dropdownTenureRI.tsx") is `false`, set it to `true`.

2. **handleOptionClick** (commented out)
   - **Parameters**: `option: string`
   - **Description**: Handles the selection of an option from the dropdown.
   - **Pseudo Logic**:
     - Set [`selectedValue`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FRISP%2FdropdownTenureRI.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FRISP%2FdropdownTenureRI.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FRISP%2FdropdownTenureRI.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A25%2C%22character%22%3A2%7D%5D "src/components/RISP/dropdownTenureRI.tsx") to the selected `option`.
     - Close the dropdown by setting [`isOpen`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FRISP%2FdropdownTenureRI.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FRISP%2FdropdownTenureRI.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FRISP%2FdropdownTenureRI.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A24%2C%22character%22%3A2%7D%5D "src/components/RISP/dropdownTenureRI.tsx") to `false`.
     - Trigger the [`onChange`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FRISP%2FdropdownTenureRI.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FRISP%2FdropdownTenureRI.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FRISP%2FdropdownTenureRI.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A8%2C%22character%22%3A2%7D%5D "src/components/RISP/dropdownTenureRI.tsx") callback with the selected `option`.

#### Component Implementation
```tsx
import React, { useState, useEffect, useRef } from "react";
import KeyboardArrowDownIcon from "@mui/icons-material/KeyboardArrowDown";
import { setTenureApi } from "../../apis/rispApis/setTenureApi";
import toast from "react-hot-toast";

interface DropdownProps {
  options: string[];
  value?: string;
  onChange: (newValue: string) => void;
  vm_id?: string;
  vm_name?: string;
  subscription_id?: string;
  location?: string;
}

const Dropdown: React.FC<DropdownProps> = ({
  options,
  value,
  onChange,
  vm_id,
  vm_name,
  subscription_id,
  location,
}) => {
  const [isOpen, setIsOpen] = useState(false);
  const [selectedValue, setSelectedValue] = useState(value || options[0]);
  const [dropdownPosition, setDropdownPosition] = useState<"bottom" | "top">(
    "bottom"
  );
  const containerRef = useRef<HTMLDivElement>(null);

  const toggleDropdown = () => {
    setIsOpen(!isOpen);
  };

  // const handleOptionClick = (option: string) => {
  //   setSelectedValue(option);

  //   setIsOpen(false);
  //   onChange(option);
  // };

  return (
    // JSX for rendering the component goes here
  );
};

export default Dropdown;
```

This documentation provides a detailed overview of the component's props, state, methods, and their respective logic.

sumitchohan: generate detailed documentation for current code file including all methods, fields, properties and summary of their implementation including pseudo logic


GitHub Copilot: ### Documentation for [`policyListRI.tsx`](command:_github.copilot.openRelativePath?%5B%7B%22scheme%22%3A%22file%22%2C%22authority%22%3A%22%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FRISP%2FpolicyListRI.tsx%22%2C%22query%22%3A%22%22%2C%22fragment%22%3A%22%22%7D%5D "/Users/sumit/Downloads/Frontend/NAGISVPR/src/components/RISP/policyListRI.tsx")

#### Summary
This file defines a React functional component [`PolicyListRI`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FRISP%2FpolicyListRI.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FRISP%2FpolicyListRI.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FRISP%2FpolicyListRI.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A27%2C%22character%22%3A0%7D%5D "src/components/RISP/policyListRI.tsx") which is responsible for displaying and managing a list of committed usage policies. It includes various functionalities such as sorting, updating comments, and handling selected tenures.

#### Imports
- **React Imports**: [`useEffect`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FRISP%2FpolicyListRI.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FRISP%2FpolicyListRI.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FRISP%2FpolicyListRI.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A0%2C%22character%22%3A0%7D%5D "src/components/RISP/policyListRI.tsx"), [`useRef`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FRISP%2FpolicyListRI.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FRISP%2FpolicyListRI.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FRISP%2FpolicyListRI.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A0%2C%22character%22%3A0%7D%5D "src/components/RISP/policyListRI.tsx"), [`useState`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FRISP%2FpolicyListRI.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FRISP%2FpolicyListRI.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FRISP%2FpolicyListRI.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A0%2C%22character%22%3A0%7D%5D "src/components/RISP/policyListRI.tsx") from React library.
- **Icons**: [`NorthIcon`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FRISP%2FpolicyListRI.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FRISP%2FpolicyListRI.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FRISP%2FpolicyListRI.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A1%2C%22character%22%3A0%7D%5D "src/components/RISP/policyListRI.tsx"), [`SouthIcon`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FRISP%2FpolicyListRI.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FRISP%2FpolicyListRI.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FRISP%2FpolicyListRI.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A2%2C%22character%22%3A0%7D%5D "src/components/RISP/policyListRI.tsx"), [`ArrowCircleUpIcon`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FRISP%2FpolicyListRI.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FRISP%2FpolicyListRI.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FRISP%2FpolicyListRI.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A5%2C%22character%22%3A0%7D%5D "src/components/RISP/policyListRI.tsx") from Material UI and [`faCircleInfo`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FRISP%2FpolicyListRI.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FRISP%2FpolicyListRI.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FRISP%2FpolicyListRI.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A8%2C%22character%22%3A0%7D%5D "src/components/RISP/policyListRI.tsx") from FontAwesome.
- **Components**: [`Dropdown`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FRISP%2FdropdownTenureRI.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FRISP%2FdropdownTenureRI.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FRISP%2FdropdownTenureRI.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A15%2C%22character%22%3A0%7D%5D "src/components/RISP/dropdownTenureRI.tsx"), [`EditableCell`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FRISP%2FcommentsRI.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FRISP%2FcommentsRI.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FRISP%2FcommentsRI.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A6%2C%22character%22%3A0%7D%5D "src/components/RISP/commentsRI.tsx"), [`CommittedUsageDiscounts`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FRISP%2FcommitedUsageDiscounts.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FRISP%2FcommitedUsageDiscounts.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FRISP%2FcommitedUsageDiscounts.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A88%2C%22character%22%3A0%7D%5D "src/components/RISP/commitedUsageDiscounts.tsx").
- **Utilities**: [`moment-timezone`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FRISP%2FpolicyListRI.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FRISP%2FpolicyListRI.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FRISP%2FpolicyListRI.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A7%2C%22character%22%3A0%7D%5D "src/components/RISP/policyListRI.tsx") for date-time manipulation.
- **APIs**: [`getCommentRI`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FgetCommentRI.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A3%2C%22character%22%3A0%7D%5D "src/apis/getCommentRI.tsx"), [`getCommittedUsage`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FgetCommitedUsage.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A3%2C%22character%22%3A0%7D%5D "src/apis/getCommitedUsage.tsx") for fetching data.
- **Tooltip**: [`Tippy`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FRISP%2FpolicyListRI.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FRISP%2FpolicyListRI.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FRISP%2FpolicyListRI.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A10%2C%22character%22%3A0%7D%5D "src/components/RISP/policyListRI.tsx") for tooltips.

#### Interfaces
- **PolicyListRIProps**
  - [`setSelectedRows`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FRISP%2FpolicyListRI.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FRISP%2FpolicyListRI.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FRISP%2FpolicyListRI.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A16%2C%22character%22%3A2%7D%5D "src/components/RISP/policyListRI.tsx"): Function to set selected rows.
  - [`setSelectedTenures`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FRISP%2FpolicyListRI.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FRISP%2FpolicyListRI.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FRISP%2FpolicyListRI.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A17%2C%22character%22%3A2%7D%5D "src/components/RISP/policyListRI.tsx"): Function to set selected tenures.
  - [`committedUsage`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FRISP%2FpolicyListRI.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FRISP%2FpolicyListRI.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FRISP%2FpolicyListRI.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A20%2C%22character%22%3A2%7D%5D "src/components/RISP/policyListRI.tsx"): Array of committed usage objects.
  - [`committedUsageList`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FRISP%2FpolicyListRI.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FRISP%2FpolicyListRI.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FRISP%2FpolicyListRI.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A21%2C%22character%22%3A2%7D%5D "src/components/RISP/policyListRI.tsx"): Array of committed usage objects.
  - [`updateCommittedUsageList`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FRISP%2FpolicyListRI.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FRISP%2FpolicyListRI.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FRISP%2FpolicyListRI.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A22%2C%22character%22%3A2%7D%5D "src/components/RISP/policyListRI.tsx"): Function to update the committed usage list.
  - [`selectedSortParam`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FRISP%2FpolicyListRI.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FRISP%2FpolicyListRI.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FRISP%2FpolicyListRI.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A23%2C%22character%22%3A2%7D%5D "src/components/RISP/policyListRI.tsx"): String indicating the selected sort parameter.
  - [`username`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FRISP%2FpolicyListRI.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FRISP%2FpolicyListRI.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FRISP%2FpolicyListRI.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A24%2C%22character%22%3A2%7D%5D "src/components/RISP/policyListRI.tsx"): String indicating the username.

#### State Variables
- **tenure_options**: Array of tenure options.
- **selectedTenures**: State to manage selected tenures.
- **checkedState**: State to manage the checked state of each committed usage item.
- **sortedData**: State to manage the sorted data.
- **sortDirection**: State to manage the sort direction (ascending or descending).

#### Methods
- **handleCommentUpdate**
  - **Parameters**: [`vm_Id`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FRISP%2FpolicyListRI.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FRISP%2FpolicyListRI.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FRISP%2FpolicyListRI.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A38%2C%22character%22%3A31%7D%5D "src/components/RISP/policyListRI.tsx") (string), [`newComment`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FRISP%2FpolicyListRI.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FRISP%2FpolicyListRI.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FRISP%2FpolicyListRI.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A38%2C%22character%22%3A46%7D%5D "src/components/RISP/policyListRI.tsx") (string)
  - **Description**: Updates the comment for a specific committed usage item.
  - **Pseudo Logic**:
    1. Map through [`props.committedUsageList`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FRISP%2FpolicyListRI.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FRISP%2FpolicyListRI.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FRISP%2FpolicyListRI.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A27%2C%22character%22%3A51%7D%5D "src/components/RISP/policyListRI.tsx").
    2. If [`item.vm_id`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FRISP%2FpolicyListRI.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FRISP%2FpolicyListRI.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FRISP%2FpolicyListRI.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A39%2C%22character%22%3A54%7D%5D "src/components/RISP/policyListRI.tsx") matches [`vm_Id`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FRISP%2FpolicyListRI.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FRISP%2FpolicyListRI.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FRISP%2FpolicyListRI.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A38%2C%22character%22%3A31%7D%5D "src/components/RISP/policyListRI.tsx"), update the [`comment`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FRISP%2FpolicyListRI.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FRISP%2FpolicyListRI.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FRISP%2FpolicyListRI.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A40%2C%22character%22%3A40%7D%5D "src/components/RISP/policyListRI.tsx") field with [`newComment`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FRISP%2FpolicyListRI.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FRISP%2FpolicyListRI.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FRISP%2FpolicyListRI.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A38%2C%22character%22%3A46%7D%5D "src/components/RISP/policyListRI.tsx").
    3. Return the updated list.

#### Pseudo Logic for Component
1. **Initialize State Variables**:
   - [`selectedTenures`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FRISP%2FpolicyListRI.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FRISP%2FpolicyListRI.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FRISP%2FpolicyListRI.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A29%2C%22character%22%3A2%7D%5D "src/components/RISP/policyListRI.tsx") as an empty object.
   - [`checkedState`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FRISP%2FpolicyListRI.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FRISP%2FpolicyListRI.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FRISP%2FpolicyListRI.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A32%2C%22character%22%3A2%7D%5D "src/components/RISP/policyListRI.tsx") as an array of booleans with the same length as [`props.committedUsage`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FRISP%2FpolicyListRI.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FRISP%2FpolicyListRI.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FRISP%2FpolicyListRI.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A27%2C%22character%22%3A51%7D%5D "src/components/RISP/policyListRI.tsx").
   - [`sortedData`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FRISP%2FpolicyListRI.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FRISP%2FpolicyListRI.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FRISP%2FpolicyListRI.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A35%2C%22character%22%3A2%7D%5D "src/components/RISP/policyListRI.tsx") as an empty array.
   - [`sortDirection`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FRISP%2FpolicyListRI.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FRISP%2FpolicyListRI.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FRISP%2FpolicyListRI.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A36%2C%22character%22%3A2%7D%5D "src/components/RISP/policyListRI.tsx") as "desc".

2. **Define [`handleCommentUpdate`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FRISP%2FpolicyListRI.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FRISP%2FpolicyListRI.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FRISP%2FpolicyListRI.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A38%2C%22character%22%3A2%7D%5D "src/components/RISP/policyListRI.tsx") Method**:
   - Takes [`vm_Id`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FRISP%2FpolicyListRI.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FRISP%2FpolicyListRI.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FRISP%2FpolicyListRI.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A38%2C%22character%22%3A31%7D%5D "src/components/RISP/policyListRI.tsx") and [`newComment`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FRISP%2FpolicyListRI.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FRISP%2FpolicyListRI.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FRISP%2FpolicyListRI.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A38%2C%22character%22%3A46%7D%5D "src/components/RISP/policyListRI.tsx") as parameters.
   - Maps through [`props.committedUsageList`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FRISP%2FpolicyListRI.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FRISP%2FpolicyListRI.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FRISP%2FpolicyListRI.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A27%2C%22character%22%3A51%7D%5D "src/components/RISP/policyListRI.tsx") to find the item with the matching `vm_id`.
   - Updates the [`comment`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FRISP%2FpolicyListRI.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FRISP%2FpolicyListRI.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FRISP%2FpolicyListRI.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A40%2C%22character%22%3A40%7D%5D "src/components/RISP/policyListRI.tsx") field of the matched item with [`newComment`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FRISP%2FpolicyListRI.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FRISP%2FpolicyListRI.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FRISP%2FpolicyListRI.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A38%2C%22character%22%3A46%7D%5D "src/components/RISP/policyListRI.tsx").
   - Returns the updated list.

3. **Render Component**:
   - Use various imported components and icons to build the UI.
   - Implement sorting, selection, and other functionalities as needed.

### Code
```tsx
import React, { useEffect, useRef, useState } from "react";
import NorthIcon from "@mui/icons-material/North";
import SouthIcon from "@mui/icons-material/South";
import Dropdown from "./dropdownTenureRI";
import EditableCell from "./commentsRI";
import ArrowCircleUpIcon from "@mui/icons-material/ArrowCircleUp";
import { CommittedUsage } from "../../utils/interfaces";
import moment from "moment-timezone";
import { faCircleInfo } from "@fortawesome/free-solid-svg-icons";
import { FontAwesomeIcon } from "@fortawesome/react-fontawesome";
import Tippy from "@tippyjs/react";
import { getCommentRI } from "../../apis/getCommentRI";
import { getCommittedUsage } from "../../apis/getCommitedUsage";
import CommittedUsageDiscounts from "./commitedUsageDiscounts";

interface PolicyListRIProps {
  setSelectedRows: React.Dispatch<React.SetStateAction<any[]>>;
  setSelectedTenures: React.Dispatch<
    React.SetStateAction<Record<string, string>>
  >;
  committedUsage: CommittedUsage[];
  committedUsageList: CommittedUsage[];
  updateCommittedUsageList: (updatedList: any) => void;
  selectedSortParam: string;
  username: string;
}

const PolicyListRI: React.FC<PolicyListRIProps> = (props) => {
  const tenure_options = ["Select", "No", "01", "03"];
  const [selectedTenures, setSelectedTenures] = useState<
    Record<string, string>
  >({});
  const [checkedState, setCheckedState] = useState<boolean[]>(
    Array(props.committedUsage.length).fill(false)
  );
  const [sortedData, setSortedData] = useState<CommittedUsage[]>([]);
  const [sortDirection, setSortDirection] = useState<"asc" | "desc">("desc");

  const handleCommentUpdate = (vm_Id: string, newComment: string) => {
    const updatedList = props.committedUsageList.map((item) =>
      item.vm_id === vm_Id ? { ...item, comment: newComment } : item
    );
    props.updateCommittedUsageList(updatedList);
  };

  // Additional component logic and rendering here

  return (
    // JSX for rendering the component
  );
};

export default PolicyListRI;
```

This documentation provides a detailed overview of the `PolicyListRI` component, including its properties, state variables, and methods, along with the pseudo logic for the main functionalities.

sumitchohan: generate detailed documentation for current code file including all methods, fields, properties and summary of their implementation including pseudo logic

GitHub Copilot: ### Documentation for [`proposedPurchase.tsx`](command:_github.copilot.openRelativePath?%5B%7B%22scheme%22%3A%22file%22%2C%22authority%22%3A%22%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FRISP%2FproposedPurchase.tsx%22%2C%22query%22%3A%22%22%2C%22fragment%22%3A%22%22%7D%5D "/Users/sumit/Downloads/Frontend/NAGISVPR/src/components/RISP/proposedPurchase.tsx")

#### Summary
This file defines a React functional component [`ProposedPurchaseView`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FRISP%2FproposedPurchase.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FRISP%2FproposedPurchase.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FRISP%2FproposedPurchase.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A11%2C%22character%22%3A0%7D%5D "src/components/RISP/proposedPurchase.tsx") which is used to display and manage proposed purchases. It includes functionality to add tenure and respective cost savings to selected rows and aggregate data for display in a table.

#### Imports
- [`React, { useState }`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FRISP%2FproposedPurchase.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FRISP%2FproposedPurchase.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FRISP%2FproposedPurchase.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A0%2C%22character%22%3A0%7D%5D "src/components/RISP/proposedPurchase.tsx") from `"react"`: Core React library and hooks.
- [`CancelIcon`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FRISP%2FproposedPurchase.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FRISP%2FproposedPurchase.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FRISP%2FproposedPurchase.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A1%2C%22character%22%3A0%7D%5D "src/components/RISP/proposedPurchase.tsx") from `"@mui/icons-material/Cancel"`: Material UI icon for cancel action.
- [`CSVLink`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FRISP%2FproposedPurchase.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FRISP%2FproposedPurchase.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FRISP%2FproposedPurchase.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A2%2C%22character%22%3A0%7D%5D "src/components/RISP/proposedPurchase.tsx") from `"react-csv"`: Component for generating CSV download links.
- `{ SelectedRow }` from `"../../utils/interfaces"`: TypeScript interface for selected rows.

#### Interfaces
- [`ProposedPurchaseViewProps`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FRISP%2FproposedPurchase.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FRISP%2FproposedPurchase.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FRISP%2FproposedPurchase.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A5%2C%22character%22%3A0%7D%5D "src/components/RISP/proposedPurchase.tsx")
  - [`onClose: () => void`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FRISP%2FproposedPurchase.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FRISP%2FproposedPurchase.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FRISP%2FproposedPurchase.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A6%2C%22character%22%3A2%7D%5D "src/components/RISP/proposedPurchase.tsx"): Callback function to close the view.
  - [`selectedRows: any[]`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FRISP%2FproposedPurchase.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FRISP%2FproposedPurchase.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FRISP%2FproposedPurchase.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A7%2C%22character%22%3A2%7D%5D "src/components/RISP/proposedPurchase.tsx"): Array of selected rows.
  - [`selectedTenures: Record<string, string>`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FRISP%2FproposedPurchase.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FRISP%2FproposedPurchase.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FRISP%2FproposedPurchase.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A8%2C%22character%22%3A2%7D%5D "src/components/RISP/proposedPurchase.tsx"): Object mapping VM IDs to their respective tenures.

#### Component: [`ProposedPurchaseView`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FRISP%2FproposedPurchase.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FRISP%2FproposedPurchase.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FRISP%2FproposedPurchase.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A11%2C%22character%22%3A0%7D%5D "src/components/RISP/proposedPurchase.tsx")
- **Props**: 
  - [`selectedRows`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FRISP%2FproposedPurchase.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FRISP%2FproposedPurchase.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FRISP%2FproposedPurchase.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A7%2C%22character%22%3A2%7D%5D "src/components/RISP/proposedPurchase.tsx"): Array of selected rows.
  - [`onClose`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FRISP%2FproposedPurchase.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FRISP%2FproposedPurchase.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FRISP%2FproposedPurchase.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A6%2C%22character%22%3A2%7D%5D "src/components/RISP/proposedPurchase.tsx"): Function to close the view.
  - [`selectedTenures`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FRISP%2FproposedPurchase.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FRISP%2FproposedPurchase.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FRISP%2FproposedPurchase.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A8%2C%22character%22%3A2%7D%5D "src/components/RISP/proposedPurchase.tsx"): Object mapping VM IDs to their respective tenures.

- **State**: None (uses props directly).

- **Logic**:
  1. **Adding Tenure and Cost Savings**:
     - Iterate over [`selectedRows`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FRISP%2FproposedPurchase.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FRISP%2FproposedPurchase.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FRISP%2FproposedPurchase.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A7%2C%22character%22%3A2%7D%5D "src/components/RISP/proposedPurchase.tsx").
     - For each row, determine the tenure using [`selectedTenures`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FRISP%2FproposedPurchase.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FRISP%2FproposedPurchase.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FRISP%2FproposedPurchase.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A8%2C%22character%22%3A2%7D%5D "src/components/RISP/proposedPurchase.tsx") and the VM ID.
     - Calculate potential cost savings based on the tenure:
       - If tenure is "01", use `annual_ri_potential_cost_saving`.
       - If tenure is "03", use one-third of `tri_annual_ri_potential_cost_saving`.
     - Return a new row object with added [`tenure`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FRISP%2FproposedPurchase.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FRISP%2FproposedPurchase.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FRISP%2FproposedPurchase.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A18%2C%22character%22%3A4%7D%5D "src/components/RISP/proposedPurchase.tsx") and [`potentialCostSavings`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FRISP%2FproposedPurchase.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FRISP%2FproposedPurchase.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FRISP%2FproposedPurchase.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A22%2C%22character%22%3A4%7D%5D "src/components/RISP/proposedPurchase.tsx").

  2. **Aggregating Data for the Table**:
     - Reduce [`rowsWithTenureAndSavings`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FRISP%2FproposedPurchase.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FRISP%2FproposedPurchase.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FRISP%2FproposedPurchase.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A17%2C%22character%22%3A2%7D%5D "src/components/RISP/proposedPurchase.tsx") to aggregate data.
     - Use a composite key (`vm_size`, `subscription_name`, `region`, [`tenure`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FRISP%2FproposedPurchase.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FRISP%2FproposedPurchase.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FRISP%2FproposedPurchase.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A18%2C%22character%22%3A4%7D%5D "src/components/RISP/proposedPurchase.tsx")) to group rows.
     - If the key does not exist in the accumulator, initialize it.
     - Aggregate data as needed (implementation not fully shown in the excerpt).

#### Pseudo Logic
```plaintext
1. Define the `ProposedPurchaseView` component.
2. Extract `selectedRows`, `onClose`, and `selectedTenures` from props.
3. Map over `selectedRows` to add tenure and potential cost savings:
   a. For each row:
      i. Determine tenure from `selectedTenures` using `vm_id`.
      ii. Initialize `potentialCostSavings` to 0.
      iii. If tenure is "01", set `potentialCostSavings` to `annual_ri_potential_cost_saving`.
      iv. If tenure is "03", set `potentialCostSavings` to one-third of `tri_annual_ri_potential_cost_saving`.
      v. Return a new row object with added `tenure` and `potentialCostSavings`.
4. Reduce `rowsWithTenureAndSavings` to aggregate data:
   a. For each row:
      i. Create a composite key using `vm_size`, `subscription_name`, `region`, and `tenure`.
      ii. If the key does not exist in the accumulator, initialize it.
      iii. Aggregate data as needed.
```

#### Code
```tsx
import React, { useState } from "react";
import CancelIcon from "@mui/icons-material/Cancel";
import { CSVLink } from "react-csv";
import { SelectedRow } from "../../utils/interfaces";

interface ProposedPurchaseViewProps {
  onClose: () => void;
  selectedRows: any[];
  selectedTenures: Record<string, string>;
}

const ProposedPurchaseView: React.FC<ProposedPurchaseViewProps> = ({
  selectedRows,
  onClose,
  selectedTenures,
}) => {
  // Function that adds tenure and respective cost saving in selectedRows
  const rowsWithTenureAndSavings = selectedRows.map((row) => {
    const tenure = selectedTenures[row.vm_id]
      ? String(selectedTenures[row.vm_id])
      : "No";

    let potentialCostSavings = 0;

    if (tenure === "01") {
      potentialCostSavings = row.annual_ri_potential_cost_saving;
    } else if (tenure === "03") {
      potentialCostSavings = (row.tri_annual_ri_potential_cost_saving / 3);
    }

    return {
      ...row,
      tenure,
      potentialCostSavings,
    };
  });

  // Function to aggregate data for the table
  const aggregatedRows = rowsWithTenureAndSavings.reduce((acc, row) => {
    const key = `${row.vm_size}-${row.subscription_name}-${row.region}-${row.tenure}`;
    if (!acc[key]) {
      // Initialize aggregation logic here
    }
    // Continue aggregation logic here
  });

  return (
    // JSX for rendering the component
  );
};

export default ProposedPurchaseView;
```

sumitchohan: generate detailed documentation for current code file including all methods, fields, properties and summary of their implementation including pseudo logic



GitHub Copilot: ### Documentation for [`accountList.tsx`](command:_github.copilot.openRelativePath?%5B%7B%22scheme%22%3A%22file%22%2C%22authority%22%3A%22%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FaccountList.tsx%22%2C%22query%22%3A%22%22%2C%22fragment%22%3A%22%22%7D%5D "/Users/sumit/Downloads/Frontend/NAGISVPR/src/components/accountList.tsx")

#### Summary
This file defines a React functional component [`AccountList`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FaccountList.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FaccountList.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FaccountList.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A20%2C%22character%22%3A0%7D%5D "src/components/accountList.tsx") that displays a list of accounts with pagination. It uses several hooks to manage state and side effects. The component also includes a method to handle pagination logic.

#### Imports
- **React**: Core library for building user interfaces.
- **useEffect, useState**: React hooks for managing side effects and state.
- **getAllAccounts**: API call to fetch all accounts.
- **Account**: TypeScript interface defining the structure of an account.
- **AccountRow**: Component to display a single account row.
- **Pagination**: Component to handle pagination controls.
- **LoaderPie**: SVG asset for a loading spinner.

#### Interfaces
- **AccountListProps**
  - [`accounts: Account[]`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FaccountList.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FaccountList.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FaccountList.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A9%2C%22character%22%3A2%7D%5D "src/components/accountList.tsx"): List of accounts to display.
  - [`cloudProvider: string`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FaccountList.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FaccountList.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FaccountList.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A10%2C%22character%22%3A2%7D%5D "src/components/accountList.tsx"): The cloud provider associated with the accounts.
  - [`setRegionSidebarOpen: (value: boolean) => void`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FaccountList.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FaccountList.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FaccountList.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A11%2C%22character%22%3A2%7D%5D "src/components/accountList.tsx"): Function to toggle the region sidebar.
  - [`setAccount?: (account: Account) => void`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FaccountList.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FaccountList.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FaccountList.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A12%2C%22character%22%3A2%7D%5D "src/components/accountList.tsx"): Optional function to set the current account.
  - [`getAccount?: (account: Account) => void`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FaccountList.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FaccountList.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FaccountList.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A13%2C%22character%22%3A2%7D%5D "src/components/accountList.tsx"): Optional function to get the current account.
  - [`setCloudProvider: (value: string) => void`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FaccountList.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FaccountList.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FaccountList.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A14%2C%22character%22%3A2%7D%5D "src/components/accountList.tsx"): Function to set the cloud provider.
  - [`setAccountAlias: (value: string) => void`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FaccountList.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FaccountList.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FaccountList.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A15%2C%22character%22%3A2%7D%5D "src/components/accountList.tsx"): Function to set the account alias.
  - [`isLoading: boolean`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FaccountList.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FaccountList.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FaccountList.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A16%2C%22character%22%3A2%7D%5D "src/components/accountList.tsx"): Flag indicating if the data is currently loading.

#### Functional Component
- **AccountList**
  - **Props**: Typed with [`AccountListProps`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FaccountList.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FaccountList.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FaccountList.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A8%2C%22character%22%3A0%7D%5D "src/components/accountList.tsx").
  - **State Variables**:
    - [`currentPage: number`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FaccountList.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FaccountList.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FaccountList.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A22%2C%22character%22%3A2%7D%5D "src/components/accountList.tsx"): Tracks the current page number for pagination.
    - [`itemsPerPage: number`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FaccountList.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FaccountList.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FaccountList.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A23%2C%22character%22%3A2%7D%5D "src/components/accountList.tsx"): Constant defining the number of items per page (set to 5).
    - [`accountDetails: Account | undefined`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FaccountList.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FaccountList.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FaccountList.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A24%2C%22character%22%3A2%7D%5D "src/components/accountList.tsx"): State to hold details of a selected account.
    - [`accountList: Account[]`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FaccountList.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FaccountList.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FaccountList.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A25%2C%22character%22%3A2%7D%5D "src/components/accountList.tsx"): State to hold the list of accounts, initialized with [`props.accounts`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FaccountList.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FaccountList.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FaccountList.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A20%2C%22character%22%3A49%7D%5D "src/components/accountList.tsx").

#### Methods
- **triggerPaginate**
  - **Purpose**: Handles the pagination logic by showing/hiding account rows based on the current page.
  - **Pseudo Logic**:
    1. Get all elements with the class name "account-row".
    2. Calculate the start and end indices for the current page.
    3. Loop through all account rows.
    4. If the row index is within the start and end range, set its class to "account-row".
    5. Otherwise, set its class to "account-row hidden".

#### Pseudo Logic for [`AccountList`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FaccountList.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FaccountList.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FaccountList.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A20%2C%22character%22%3A0%7D%5D "src/components/accountList.tsx") Component
1. **Initialize State**:
   - [`currentPage`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FaccountList.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FaccountList.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FaccountList.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A22%2C%22character%22%3A2%7D%5D "src/components/accountList.tsx") to 0.
   - [`itemsPerPage`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FaccountList.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FaccountList.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FaccountList.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A23%2C%22character%22%3A2%7D%5D "src/components/accountList.tsx") to 5.
   - [`accountDetails`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FaccountList.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FaccountList.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FaccountList.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A24%2C%22character%22%3A2%7D%5D "src/components/accountList.tsx") to undefined.
   - [`accountList`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FaccountList.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FaccountList.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FaccountList.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A25%2C%22character%22%3A2%7D%5D "src/components/accountList.tsx") to [`props.accounts`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FaccountList.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FaccountList.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FaccountList.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A20%2C%22character%22%3A49%7D%5D "src/components/accountList.tsx").

2. **Define [`triggerPaginate`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FaccountList.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FaccountList.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FaccountList.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A27%2C%22character%22%3A2%7D%5D "src/components/accountList.tsx") Method**:
   - Get all elements with the class "account-row".
   - Calculate the start and end indices based on [`currentPage`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FaccountList.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FaccountList.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FaccountList.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A22%2C%22character%22%3A2%7D%5D "src/components/accountList.tsx") and [`itemsPerPage`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FaccountList.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FaccountList.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FaccountList.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A23%2C%22character%22%3A2%7D%5D "src/components/accountList.tsx").
   - Loop through the elements and set their class to "account-row" if they fall within the range, otherwise set to "account-row hidden".

3. **Render Component**:
   - Display account rows and pagination controls.
   - Use [`triggerPaginate`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FaccountList.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FaccountList.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FaccountList.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A27%2C%22character%22%3A2%7D%5D "src/components/accountList.tsx") to manage the visibility of account rows based on the current page.

### Code
```tsx
// Import React
import React, { useEffect, useState } from "react";
import { getAllAccounts } from "../apis/accountAPI";
import { Account } from "../utils/interfaces";
import AccountRow from "./accountRow";
import Pagination from "./pagination";
import LoaderPie from "./../assets/loader.svg";

interface AccountListProps {
  accounts: Account[];
  cloudProvider: string;
  setRegionSidebarOpen: (value: boolean) => void;
  setAccount?: (account: Account) => void;
  getAccount?: (account: Account) => void;
  setCloudProvider: (value: string) => void;
  setAccountAlias: (value: string) => void;
  isLoading: boolean;
}

// Functional component with typed props
const AccountList: React.FC<AccountListProps> = (props) => {
  const [currentPage, setCurrentPage] = useState<number>(0);
  const itemsPerPage = 5;
  const [accountDetails, setAccountDetails] = useState<Account>();
  const [accountList, setAccountList] = useState<Account[]>(props.accounts);

  const triggerPaginate = () => {
    let tbody = document.getElementsByClassName("account-row");
    let start = (currentPage - 1) * itemsPerPage + 1;
    let end = start + itemsPerPage;
    for (let i = 1; i <= tbody.length; i++) {
      if (start <= i && i < end) {
        tbody[i - 1].className = "account-row";
      } else {
        tbody[i - 1].className = "account-row hidden";
      }
    }
    return "";
  };
};
```

sumitchohan: generate detailed documentation for current code file including all methods, fields, properties and summary of their implementation including pseudo logic





GitHub Copilot: ### Documentation for [`accountRow.tsx`](command:_github.copilot.openRelativePath?%5B%7B%22scheme%22%3A%22file%22%2C%22authority%22%3A%22%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FaccountRow.tsx%22%2C%22query%22%3A%22%22%2C%22fragment%22%3A%22%22%7D%5D "/Users/sumit/Downloads/Frontend/NAGISVPR/src/components/accountRow.tsx")

#### Summary
This file defines a React functional component [`AccountRow`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FaccountRow.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FaccountRow.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FaccountRow.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A32%2C%22character%22%3A0%7D%5D "src/components/accountRow.tsx") which represents a row in a list of accounts. It includes various states and methods to handle the display and actions related to an account, such as showing environments, deleting the account, and navigating to different views.

#### Imports
- **React Hooks**: [`useEffect`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FaccountRow.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FaccountRow.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FaccountRow.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A0%2C%22character%22%3A0%7D%5D "src/components/accountRow.tsx"), [`useRef`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FaccountRow.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FaccountRow.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FaccountRow.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A0%2C%22character%22%3A0%7D%5D "src/components/accountRow.tsx"), [`useState`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FaccountRow.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FaccountRow.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FaccountRow.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A0%2C%22character%22%3A0%7D%5D "src/components/accountRow.tsx") - Used for managing component lifecycle and state.
- **Interfaces**: [`Policy`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Futils%2Finterfaces.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A9%2C%22character%22%3A0%7D%5D "src/utils/interfaces.tsx"), [`ReportResponseFormat`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Futils%2Finterfaces.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A46%2C%22character%22%3A0%7D%5D "src/utils/interfaces.tsx"), [`Account`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Futils%2Finterfaces.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A81%2C%22character%22%3A0%7D%5D "src/utils/interfaces.tsx") - Type definitions for various objects.
- **Assets**: [`LoaderPie`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FaccountRow.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FaccountRow.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FaccountRow.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A3%2C%22character%22%3A0%7D%5D "src/components/accountRow.tsx") - SVG loader image.
- **Utilities**: [`getDate`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Futils%2FdateUtils.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A0%2C%22character%22%3A0%7D%5D "src/utils/dateUtils.tsx") - Utility function for date manipulation.
- **FontAwesome Icons**: [`faAngleDown`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FaccountRow.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FaccountRow.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FaccountRow.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A7%2C%22character%22%3A0%7D%5D "src/components/accountRow.tsx"), [`faEllipsisVertical`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FaccountRow.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FaccountRow.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FaccountRow.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A7%2C%22character%22%3A0%7D%5D "src/components/accountRow.tsx"), [`faPlus`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FaccountRow.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FaccountRow.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FaccountRow.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A7%2C%22character%22%3A0%7D%5D "src/components/accountRow.tsx"), [`faTrash`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FaccountRow.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FaccountRow.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FaccountRow.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A7%2C%22character%22%3A0%7D%5D "src/components/accountRow.tsx") - Icons used in the component.
- **Components**: [`EnviromentList`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FenviromentList.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A17%2C%22character%22%3A0%7D%5D "src/components/enviromentList.tsx"), [`Loader`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2Floader.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A7%2C%22character%22%3A0%7D%5D "src/components/loader.tsx"), [`CustomConfirm`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FconfirmationPop.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A14%2C%22character%22%3A0%7D%5D "src/components/confirmationPop.tsx"), [`SuccessConfirm`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FsuccessConfirmPop.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A9%2C%22character%22%3A0%7D%5D "src/components/successConfirmPop.tsx") - Custom components used within [`AccountRow`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FaccountRow.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FaccountRow.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FaccountRow.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A32%2C%22character%22%3A0%7D%5D "src/components/accountRow.tsx").
- **APIs**: [`deleteAccount`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FdeleteAccountAPI.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A21%2C%22character%22%3A0%7D%5D "src/apis/deleteAccountAPI.tsx") - API call to delete an account.
- **Notifications**: [`toast`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FaccountRow.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FaccountRow.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FaccountRow.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A15%2C%22character%22%3A0%7D%5D "src/components/accountRow.tsx") - Library for showing toast notifications.
- **Routing**: [`useNavigate`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FaccountRow.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FaccountRow.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FaccountRow.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A17%2C%22character%22%3A0%7D%5D "src/components/accountRow.tsx") - Hook for navigation.
- **Confirmation**: [`confirmAlert`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FaccountRow.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FaccountRow.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FaccountRow.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A18%2C%22character%22%3A0%7D%5D "src/components/accountRow.tsx") - Library for showing confirmation dialogs.

#### Interfaces
- **AccountRowProps**
  - [`account: Account`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FaccountRow.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FaccountRow.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FaccountRow.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A24%2C%22character%22%3A2%7D%5D "src/components/accountRow.tsx") - The account object.
  - [`cloudProvider: string`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FaccountRow.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FaccountRow.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FaccountRow.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A25%2C%22character%22%3A2%7D%5D "src/components/accountRow.tsx") - The cloud provider associated with the account.
  - [`setRegionSidebarOpen: (value: boolean) => void`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FaccountRow.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FaccountRow.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FaccountRow.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A26%2C%22character%22%3A2%7D%5D "src/components/accountRow.tsx") - Function to toggle the region sidebar.
  - [`setAccount: (account: any) => void`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FaccountRow.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FaccountRow.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FaccountRow.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A27%2C%22character%22%3A2%7D%5D "src/components/accountRow.tsx") - Function to set the account.
  - [`setCloudProvider: (value: string) => void`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FaccountRow.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FaccountRow.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FaccountRow.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A28%2C%22character%22%3A2%7D%5D "src/components/accountRow.tsx") - Function to set the cloud provider.
  - [`setAccountAlias: (value: string) => void`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FaccountRow.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FaccountRow.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FaccountRow.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A29%2C%22character%22%3A2%7D%5D "src/components/accountRow.tsx") - Function to set the account alias.

#### Component: [`AccountRow`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FaccountRow.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FaccountRow.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FaccountRow.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A32%2C%22character%22%3A0%7D%5D "src/components/accountRow.tsx")
- **State Variables**
  - [`showEnviroment: boolean`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FaccountRow.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FaccountRow.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FaccountRow.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A33%2C%22character%22%3A2%7D%5D "src/components/accountRow.tsx") - Controls the visibility of the environment list.
  - [`showLoader: boolean`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FaccountRow.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FaccountRow.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FaccountRow.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A34%2C%22character%22%3A2%7D%5D "src/components/accountRow.tsx") - Controls the visibility of the loader.
  - [`loaderMessage: string`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FaccountRow.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FaccountRow.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FaccountRow.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A35%2C%22character%22%3A2%7D%5D "src/components/accountRow.tsx") - Message displayed in the loader.
  - [`showAlert: boolean`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FaccountRow.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FaccountRow.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FaccountRow.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A36%2C%22character%22%3A2%7D%5D "src/components/accountRow.tsx") - Controls the visibility of the alert.
  - [`refresh: boolean`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FaccountRow.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FaccountRow.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FaccountRow.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A37%2C%22character%22%3A2%7D%5D "src/components/accountRow.tsx") - Triggers a refresh of the component.
  - [`showConfirm: boolean`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FaccountRow.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FaccountRow.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FaccountRow.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A40%2C%22character%22%3A2%7D%5D "src/components/accountRow.tsx") - Controls the visibility of the confirmation dialog.

- **Hooks**
  - [`useNavigate`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FaccountRow.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FaccountRow.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FaccountRow.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A17%2C%22character%22%3A0%7D%5D "src/components/accountRow.tsx") - Used for navigation.

#### Pseudo Logic
1. **Initialization**
   - Import necessary modules, components, and hooks.
   - Define the [`AccountRowProps`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FaccountRow.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FaccountRow.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FaccountRow.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A23%2C%22character%22%3A0%7D%5D "src/components/accountRow.tsx") interface for the component props.

2. **Component Definition**
   - Define the [`AccountRow`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FaccountRow.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FaccountRow.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FaccountRow.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A32%2C%22character%22%3A0%7D%5D "src/components/accountRow.tsx") functional component.
   - Initialize state variables using [`useState`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FaccountRow.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FaccountRow.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FaccountRow.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A0%2C%22character%22%3A0%7D%5D "src/components/accountRow.tsx"):
     - [`showEnviroment`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FaccountRow.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FaccountRow.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FaccountRow.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A33%2C%22character%22%3A2%7D%5D "src/components/accountRow.tsx") to control environment list visibility.
     - [`showLoader`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FaccountRow.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FaccountRow.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FaccountRow.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A34%2C%22character%22%3A2%7D%5D "src/components/accountRow.tsx") to control loader visibility.
     - [`loaderMessage`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FaccountRow.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FaccountRow.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FaccountRow.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A35%2C%22character%22%3A2%7D%5D "src/components/accountRow.tsx") to set the loader message.
     - [`showAlert`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FaccountRow.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FaccountRow.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FaccountRow.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A36%2C%22character%22%3A2%7D%5D "src/components/accountRow.tsx") to control alert visibility.
     - [`refresh`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FaccountRow.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FaccountRow.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FaccountRow.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A37%2C%22character%22%3A2%7D%5D "src/components/accountRow.tsx") to trigger component refresh.
     - [`showConfirm`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FaccountRow.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FaccountRow.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FaccountRow.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A40%2C%22character%22%3A2%7D%5D "src/components/accountRow.tsx") to control confirmation dialog visibility.
   - Initialize [`navigate`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FaccountRow.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FaccountRow.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FaccountRow.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A38%2C%22character%22%3A2%7D%5D "src/components/accountRow.tsx") using [`useNavigate`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FaccountRow.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FaccountRow.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FaccountRow.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A17%2C%22character%22%3A0%7D%5D "src/components/accountRow.tsx").

3. **Component Logic**
   - Define methods and handlers for various actions (e.g., showing environments, deleting account).
   - Use imported components and utilities to build the UI and handle actions.

#### Example Code
```tsx
import { useEffect, useRef, useState } from "react";
import { Policy, ReportResponseFormat } from "../utils/interfaces";
import { Account } from "../utils/interfaces";
import LoaderPie from "./../assets/loader.svg";
import { getDate } from "../utils/dateUtils";

import { FontAwesomeIcon } from "@fortawesome/react-fontawesome";
import {
  faAngleDown,
  faEllipsisVertical,
  faPlus,
  faTrash,
} from "@fortawesome/free-solid-svg-icons";
import EnviromentList from "./enviromentList";
import { deleteAccount } from "../apis/deleteAccountAPI";
import toast from "react-hot-toast";
import Loader from "./loader";
import { useNavigate } from "react-router-dom";
import { confirmAlert } from "react-confirm-alert";
import "react-confirm-alert/src/react-confirm-alert.css";
import CustomConfirm from "./confirmationPop";
import SuccessConfirm from "./successConfirmPop";

interface AccountRowProps {
  account: Account;
  cloudProvider: string;
  setRegionSidebarOpen: (value: boolean) => void;
  setAccount: (account: any) => void;
  setCloudProvider: (value: string) => void;
  setAccountAlias: (value: string) => void;
}

const AccountRow: React.FC<AccountRowProps> = (props) => {
  const [showEnviroment, setShowEnviroment] = useState<boolean>(false);
  const [showLoader, setShowLoader] = useState<boolean>(false);
  const [loaderMessage, setLoaderMessage] = useState<string>("Deleting ...");
  const [showAlert, setShowAlert] = useState(false);
  const [refresh, setRefresh] = useState(false);
  const navigate = useNavigate();

  const [showConfirm, setShowConfirm] = useState(false);

  // Additional methods and handlers would be defined here
};
```

This documentation provides a detailed overview of the `accountRow.tsx` file, including its imports, interfaces, component definition, state variables, and pseudo logic.

sumitchohan: generate detailed documentation for current code file including all methods, fields, properties and summary of their implementation including pseudo logic




GitHub Copilot: ### Documentation for [`accountSetupForm.tsx`](command:_github.copilot.openRelativePath?%5B%7B%22scheme%22%3A%22file%22%2C%22authority%22%3A%22%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FaccountSetupForm.tsx%22%2C%22query%22%3A%22%22%2C%22fragment%22%3A%22%22%7D%5D "/Users/sumit/Downloads/Frontend/NAGISVPR/src/components/accountSetupForm.tsx")

#### Summary
This file defines a React component for an account setup form. It includes various UI elements such as text fields, radio buttons, and icons. The form allows users to select an AWS region from a predefined list of options.

#### Imports
- **FontAwesomeIcon**: Used to display icons from FontAwesome.
- **Material-UI Components**: Various form control components from Material-UI.
- **React**: Core library for building the component.
- **IconConsole**: An SVG icon imported from local assets.
- **CopyToClipboard**: A component to facilitate copying text to the clipboard.

#### Interfaces
- **AccountSetupFormProps**
  - **onNext**: Function to handle the next action.
  - **onPrev**: Function to handle the previous action.

#### Constants
- **SectionQuestion**
  - An array of objects representing questions and their options.
  - Each object contains:
    - **id**: Unique identifier for the question.
    - **question**: The question text.
    - **response**: Placeholder for the user's response.
    - **responseType**: Type of response expected (e.g., single choice, multiple choice).
    - **options**: An array of options for the question, each with an [`id`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FaccountSetupForm.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FaccountSetupForm.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FaccountSetupForm.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A22%2C%22character%22%3A4%7D%5D "src/components/accountSetupForm.tsx") and [`option`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FaccountSetupForm.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FaccountSetupForm.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FaccountSetupForm.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A27%2C%22character%22%3A15%7D%5D "src/components/accountSetupForm.tsx") text.

#### Pseudo Logic
1. **Imports**: Import necessary libraries and components.
2. **Interface Definition**: Define the [`AccountSetupFormProps`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FaccountSetupForm.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FaccountSetupForm.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FaccountSetupForm.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A14%2C%22character%22%3A0%7D%5D "src/components/accountSetupForm.tsx") interface to specify the props expected by the component.
3. **Constants Definition**: Define the [`SectionQuestion`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FaccountSetupForm.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FaccountSetupForm.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FaccountSetupForm.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A20%2C%22character%22%3A0%7D%5D "src/components/accountSetupForm.tsx") constant to hold the questions and options for the form.
4. **Component Definition**: Define the main component `AccountSetupForm` (not shown in the provided excerpt but implied).
   - **State Management**: Use React's [`useState`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FaccountSetupForm.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FaccountSetupForm.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FaccountSetupForm.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A10%2C%22character%22%3A0%7D%5D "src/components/accountSetupForm.tsx") to manage form state.
   - **Event Handlers**: Define functions to handle form events such as next, previous, and option selection.
   - **Render Method**: Render the form elements using Material-UI components and map over [`SectionQuestion`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FaccountSetupForm.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FaccountSetupForm.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FaccountSetupForm.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A20%2C%22character%22%3A0%7D%5D "src/components/accountSetupForm.tsx") to display each question and its options.

#### Example Code
```tsx
import { faMagnifyingGlass } from "@fortawesome/free-solid-svg-icons";
import { FontAwesomeIcon } from "@fortawesome/react-fontawesome";
import {
  FormControl,
  FormControlLabel,
  InputAdornment,
  Radio,
  RadioGroup,
  TextField,
} from "@mui/material";
import React, { useState } from "react";
import IconConsole from "./../assets/icons/icon-console.svg";
import { CopyToClipboard } from "react-copy-to-clipboard";

interface AccountSetupFormProps {
  onNext: () => void;
  onPrev: () => void;
}

const SectionQuestion = [
  {
    id: "QS101",
    question: "Select AWS Region",
    response: null,
    responseType: 1,
    options: [
      { id: 1, option: "us-east-1 - US East (N. Virginia)" },
      { id: 2, option: "us-east-2 - US East (Ohio)" },
      { id: 3, option: "us-west-1 - US West (N. California)" },
      { id: 4, option: "us-west-2 - US West (Oregon)" },
      { id: 5, option: "me-south-1 - Middle East (Bahrain)" },
      { id: 6, option: "sa-east-1 - South America (Sao Paulo)" },
      { id: 7, option: "ap-south-1 - Asia Pacific (Mumbai)" },
      { id: 8, option: "ap-northeast-1 - Asia Pacific (Tokyo)" },
      { id: 9, option: "ap-northeast-2 - Asia Pacific (Seoul)" },
      { id: 10, option: "ap-southeast-1 - Asia Pacific (Singapore)" },
      { id: 11, option: "ap-southeast-2 - Asia Pacific (Sydney)" },
      { id: 12, option: "ca-central-1 - Canada (Central)" },
      { id: 13, option: "eu-central-1 - Europe (Frankfurt)" },
      { id: 14, option: "eu-west-1 - Europe (Ireland)" },
    ],
  },
  // Additional questions can be added here
];
```

This documentation provides an overview of the file's structure and functionality, detailing the imports, interfaces, constants, and the pseudo logic for the component's implementation.

sumitchohan: generate detailed documentation for current code file including all methods, fields, properties and summary of their implementation including pseudo logic

GitHub Copilot: ### File: [`addRegion.tsx`](command:_github.copilot.openRelativePath?%5B%7B%22scheme%22%3A%22file%22%2C%22authority%22%3A%22%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FaddRegion.tsx%22%2C%22query%22%3A%22%22%2C%22fragment%22%3A%22%22%7D%5D "/Users/sumit/Downloads/Frontend/NAGISVPR/src/components/addRegion.tsx")

#### Summary
This file is a React component that provides functionality for adding a region to an account. It imports various dependencies, including icons, CSS, interfaces, APIs, and UI components. The component uses state management and side effects to handle user interactions and API calls.

#### Imports
- **FontAwesomeIcon**: Used for rendering icons.
- **CSS**: Imports styles from [`PolicyTray.css`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents-css%2FPolicyTray.css%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A0%2C%22character%22%3A0%7D%5D "src/components-css/PolicyTray.css").
- **Icons**: Imports specific icons from FontAwesome.
- **Interfaces**: Imports [`Account`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Futils%2Finterfaces.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A81%2C%22character%22%3A0%7D%5D "src/utils/interfaces.tsx"), [`Policy`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Futils%2Finterfaces.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A9%2C%22character%22%3A0%7D%5D "src/utils/interfaces.tsx"), and [`Region`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Futils%2Finterfaces.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A73%2C%22character%22%3A0%7D%5D "src/utils/interfaces.tsx") interfaces.
- **CSVLink**: Used for CSV export functionality.
- **React Hooks**: [`ChangeEvent`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FaddRegion.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FaddRegion.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FaddRegion.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A9%2C%22character%22%3A0%7D%5D "src/components/addRegion.tsx"), [`useEffect`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FaddRegion.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FaddRegion.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FaddRegion.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A9%2C%22character%22%3A0%7D%5D "src/components/addRegion.tsx"), [`useState`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FaddRegion.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FaddRegion.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FaddRegion.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A9%2C%22character%22%3A0%7D%5D "src/components/addRegion.tsx") for managing state and side effects.
- **API Functions**: [`creatJiraTicket`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FjiraTicketAPI.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A24%2C%22character%22%3A0%7D%5D "src/apis/jiraTicketAPI.tsx") and [`AddRegionApi`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FAddRegionAPI.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A55%2C%22character%22%3A0%7D%5D "src/apis/AddRegionAPI.tsx") for API interactions.
- **Material UI Components**: Various components for form controls and inputs.
- **Toast**: For displaying notifications.

#### Interface: [`AddRegionProps`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FaddRegion.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FaddRegion.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FaddRegion.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A24%2C%22character%22%3A0%7D%5D "src/components/addRegion.tsx")
Defines the properties expected by the `AddRegion` component:
- **isOpen**: `boolean` - Indicates if the component is open.
- **account**: `string` - The account identifier.
- **policy**: [`Policy | undefined`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Futils%2Finterfaces.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A9%2C%22character%22%3A0%7D%5D "src/utils/interfaces.tsx") - The policy object, which can be undefined.
- **report**: `any` - The report data.
- **getAccountId**: [`Account`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Futils%2Finterfaces.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A81%2C%22character%22%3A0%7D%5D "src/utils/interfaces.tsx") - Function to get the account ID.
- **onClose**: `() => void` - Function to close the component.
- **addRegion**: `(account_id: string, regionList: string[]) => void` - Function to add regions to an account.
- **accountList**: [`Account[]`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Futils%2Finterfaces.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A81%2C%22character%22%3A0%7D%5D "src/utils/interfaces.tsx") - List of accounts.
- **setAccountList**: `(value: Account[]) => void` - Function to set the account list.

#### Constant: [`SectionQuestion`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FaddRegion.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FaddRegion.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FaddRegion.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A36%2C%22character%22%3A0%7D%5D "src/components/addRegion.tsx")
An array of objects representing questions for the section:
- **id**: `string` - The question identifier.
- **question**: `string` - The question text.
- **response**: `null` - Placeholder for the response.

### Pseudo Logic
1. **Imports**: Import necessary modules, components, and styles.
2. **Interface Definition**: Define the [`AddRegionProps`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FaddRegion.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FaddRegion.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FaddRegion.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A24%2C%22character%22%3A0%7D%5D "src/components/addRegion.tsx") interface to specify the expected props.
3. **Constant Definition**: Define [`SectionQuestion`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FaddRegion.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FaddRegion.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FaddRegion.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A36%2C%22character%22%3A0%7D%5D "src/components/addRegion.tsx") to hold questions related to the section.
4. **Component Implementation**:
   - **State Management**: Use [`useState`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FaddRegion.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FaddRegion.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FaddRegion.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A9%2C%22character%22%3A0%7D%5D "src/components/addRegion.tsx") to manage component state.
   - **Effect Handling**: Use [`useEffect`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FaddRegion.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FaddRegion.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FaddRegion.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A9%2C%22character%22%3A0%7D%5D "src/components/addRegion.tsx") to handle side effects, such as fetching data or updating the UI based on state changes.
   - **Event Handlers**: Define functions to handle user interactions, such as form submissions, input changes, and button clicks.
   - **API Calls**: Use imported API functions to interact with backend services.
   - **UI Rendering**: Use Material UI components and FontAwesome icons to build the user interface.
   - **Notifications**: Use [`toast`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FaddRegion.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FaddRegion.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FaddRegion.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A22%2C%22character%22%3A0%7D%5D "src/components/addRegion.tsx") to display success or error messages based on user actions or API responses.

### Example Code Block
```tsx
import { FontAwesomeIcon } from "@fortawesome/react-fontawesome";
import "../components-css/PolicyTray.css";
import {
  faCircleXmark,
  faFileCsv,
  faMagnifyingGlass,
} from "@fortawesome/free-solid-svg-icons";
import { Account, Policy, Region } from "../utils/interfaces";
import { CSVLink } from "react-csv";
import { ChangeEvent, useEffect, useState } from "react";
import { creatJiraTicket } from "../apis/jiraTicketAPI";
import {
  Checkbox,
  FormControl,
  FormControlLabel,
  FormGroup,
  InputAdornment,
  Radio,
  RadioGroup,
  TextField,
} from "@mui/material";
import { AddRegionApi } from "../apis/AddRegionAPI";
import toast from "react-hot-toast";

interface AddRegionProps {
  isOpen: boolean;
  account: string;
  policy: Policy | undefined;
  report: any;
  getAccountId: Account;
  onClose: () => void;
  addRegion: (account_id: string, regionList: string[]) => void;
  accountList: Account[];
  setAccountList: (value: Account[]) => void;
}

const SectionQuestion = [
  {
    id: "QS101",
    question: "Select AWS Region",
    response: null,
  },
];
```

This documentation provides a detailed overview of the `addRegion.tsx` file, including its imports, interface definitions, constants, and a summary of the component's implementation.

sumitchohan: generate detailed documentation for current code file including all methods, fields, properties and summary of their implementation including pseudo logic



GitHub Copilot: ### Documentation for [`awsAccountSetup.tsx`](command:_github.copilot.openRelativePath?%5B%7B%22scheme%22%3A%22file%22%2C%22authority%22%3A%22%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FawsAccountSetup.tsx%22%2C%22query%22%3A%22%22%2C%22fragment%22%3A%22%22%7D%5D "/Users/sumit/Downloads/Frontend/NAGISVPR/src/components/awsAccountSetup.tsx")

#### Summary
This file defines a React component for setting up an AWS account. It includes several sub-components and interfaces to manage the setup process, including navigation between steps and displaying the current, next, and previous steps.

#### Imports
- **React, useState**: Core React library and hook for managing state.
- **AccountSetupForm**: Custom component for the account setup form.
- **AwsCLI**: Custom component for AWS CLI setup.
- **SelectRegion**: Custom component for selecting AWS region.
- **EnviromentAlias**: Custom component for setting environment alias.
- **SelectMethod**: Custom component for selecting the method of setup.
- **Navigate, useNavigate**: React Router components for navigation.
- **SelectCloud**: Custom component for selecting the cloud provider.
- **AwsConsole**: Custom component for AWS console setup.
- **AccountAlias**: Custom component for setting account alias.

#### Interfaces
- **AwsAccountSetupProps**
  - **onNext**: Function to handle the next step.
  - **onPrev**: Function to handle the previous step.
  
- **SelectedProps**
  - **number**: String representing the step number.

#### Components
1. **Current**
   - **Props**: [`SelectedProps`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FawsAccountSetup.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FawsAccountSetup.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FawsAccountSetup.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A16%2C%22character%22%3A0%7D%5D "src/components/awsAccountSetup.tsx")
   - **Description**: Displays the current step in the setup process.
   - **Pseudo Logic**:
     - Render a div with specific styling to indicate the current step.
     - Display the step number inside a styled div.

2. **Next**
   - **Props**: [`SelectedProps`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FawsAccountSetup.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FawsAccountSetup.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FawsAccountSetup.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A16%2C%22character%22%3A0%7D%5D "src/components/awsAccountSetup.tsx")
   - **Description**: Displays the next step in the setup process.
   - **Pseudo Logic**:
     - Render a div with specific styling to indicate the next step.
     - Display the step number inside a styled div.

3. **Previous**
   - **Props**: [`SelectedProps`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FawsAccountSetup.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FawsAccountSetup.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FawsAccountSetup.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A16%2C%22character%22%3A0%7D%5D "src/components/awsAccountSetup.tsx")
   - **Description**: Displays the previous step in the setup process.
   - **Pseudo Logic**:
     - Render a div with specific styling to indicate the previous step.
     - Display the step number inside a styled div.

### Pseudo Logic for Each Component

#### Current Component
```plaintext
1. Define a functional component `Current` that takes `number` as a prop.
2. Return a div with the following properties:
   - Flexbox layout to center content.
   - Height and width of 12 units.
   - Border with 2 units of spacing and green color.
   - Background color white.
   - Rounded corners.
3. Inside the div, render another div with:
   - Background color green.
   - Text color white.
   - Font size 2xl and medium weight.
   - Height and width of 10 units.
   - Rounded corners.
   - Flexbox layout to center content.
4. Display the `number` prop inside the inner div.
```

#### Next Component
```plaintext
1. Define a functional component [`Next`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FawsAccountSetup.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FawsAccountSetup.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FawsAccountSetup.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A29%2C%22character%22%3A0%7D%5D "src/components/awsAccountSetup.tsx") that takes [`number`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FawsAccountSetup.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FawsAccountSetup.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FawsAccountSetup.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A17%2C%22character%22%3A2%7D%5D "src/components/awsAccountSetup.tsx") as a prop.
2. Return a div with the following properties:
   - Flexbox layout to center content.
   - Height and width of 14 units.
   - Border with 2 units of spacing and transparent color.
   - Background color transparent.
   - Rounded corners.
3. Inside the div, render another div with:
   - Background color gray.
   - Text color gray.
   - Font size 2xl and medium weight.
   - Height and width of 12 units.
   - Rounded corners.
   - Flexbox layout to center content.
4. Display the [`number`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FawsAccountSetup.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FawsAccountSetup.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FawsAccountSetup.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A17%2C%22character%22%3A2%7D%5D "src/components/awsAccountSetup.tsx") prop inside the inner div.
```

#### Previous Component
```plaintext
1. Define a functional component `Previous` that takes `number` as a prop.
2. Return a div with the following properties:
   - Flexbox layout to center content.
   - Height and width of 12 units.
   - Border with 2 units of spacing and green color.
   - Background color white.
   - Rounded corners.
3. Inside the div, render another div with:
   - Background color green.
   - Text color white.
   - Font size 2xl and medium weight.
   - Height and width of 10 units.
   - Rounded corners.
   - Flexbox layout to center content.
4. Display the `number` prop inside the inner div.
```

This documentation provides a detailed overview of the components and their implementation logic in the `awsAccountSetup.tsx` file.

sumitchohan: generate detailed documentation for current code file including all methods, fields, properties and summary of their implementation including pseudo logic

GitHub Copilot: ### Documentation for [`awsCLI.tsx`](command:_github.copilot.openRelativePath?%5B%7B%22scheme%22%3A%22file%22%2C%22authority%22%3A%22%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FawsCLI.tsx%22%2C%22query%22%3A%22%22%2C%22fragment%22%3A%22%22%7D%5D "/Users/sumit/Downloads/Frontend/NAGISVPR/src/components/awsCLI.tsx")

#### Summary
This file defines a React functional component [`AwsCLI`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FawsCLI.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FawsCLI.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FawsCLI.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A18%2C%22character%22%3A0%7D%5D "src/components/awsCLI.tsx") which provides functionality related to AWS CLI operations. It includes state management, side effects using hooks, and a copy-to-clipboard feature.

#### Imports
- **[`unicode`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FawsCLI.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FawsCLI.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FawsCLI.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A0%2C%22character%22%3A0%7D%5D "src/components/awsCLI.tsx")**: Imported from `@fortawesome/free-regular-svg-icons/faAddressBook`.
- **[`React`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FawsCLI.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FawsCLI.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FawsCLI.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A1%2C%22character%22%3A0%7D%5D "src/components/awsCLI.tsx")**: Core React library for building UI components.
- **[`useEffect`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FawsCLI.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FawsCLI.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FawsCLI.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A1%2C%22character%22%3A0%7D%5D "src/components/awsCLI.tsx"), [`useState`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FawsCLI.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FawsCLI.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FawsCLI.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A1%2C%22character%22%3A0%7D%5D "src/components/awsCLI.tsx")**: React hooks for managing side effects and state.
- **[`CopyToClipboard`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FawsCLI.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FawsCLI.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FawsCLI.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A2%2C%22character%22%3A0%7D%5D "src/components/awsCLI.tsx")**: Component for copying text to the clipboard.
- **[`createAccountStackCheck`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FcreateAccountCheckStack.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A20%2C%22character%22%3A0%7D%5D "src/apis/createAccountCheckStack.tsx")**: API function for checking account stack.
- **[`create`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FawsCLI.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FawsCLI.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FawsCLI.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A4%2C%22character%22%3A0%7D%5D "src/components/awsCLI.tsx")**: Function from `@mui/material/styles/createTransitions`.
- **[`useNavigate`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FawsCLI.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FawsCLI.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FawsCLI.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A5%2C%22character%22%3A0%7D%5D "src/components/awsCLI.tsx")**: Hook from `react-router-dom` for navigation.
- **[`getRegistedUser`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fservices%2FauthService.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A20%2C%22character%22%3A0%7D%5D "src/services/authService.tsx")**: Service function to get registered user details.
- **[`User`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Futils%2Finterfaces.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A50%2C%22character%22%3A0%7D%5D "src/utils/interfaces.tsx")**: Interface for user data.
- **[`createStackFuncAPI`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FcreateStackAPI.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A27%2C%22character%22%3A0%7D%5D "src/apis/createStackAPI.tsx")**: API function to create stack.
- **[`getConfiguration`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FloginAPI.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A3%2C%22character%22%3A0%7D%5D "src/apis/loginAPI.tsx")**: API function to get configuration details.

#### Interfaces
- **[`AwsCLIProps`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FawsCLI.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FawsCLI.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FawsCLI.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A11%2C%22character%22%3A0%7D%5D "src/components/awsCLI.tsx")**: Defines the properties for the [`AwsCLI`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FawsCLI.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FawsCLI.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FawsCLI.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A18%2C%22character%22%3A0%7D%5D "src/components/awsCLI.tsx") component.
  - [`onCopy?`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FawsCLI.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FawsCLI.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FawsCLI.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A12%2C%22character%22%3A2%7D%5D "src/components/awsCLI.tsx"): Optional callback function triggered on copy.
  - [`uniqueId?`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FawsCLI.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FawsCLI.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FawsCLI.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A13%2C%22character%22%3A2%7D%5D "src/components/awsCLI.tsx"): Optional unique identifier.
  - [`externalId?`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FawsCLI.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FawsCLI.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FawsCLI.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A14%2C%22character%22%3A2%7D%5D "src/components/awsCLI.tsx"): Optional external identifier.
  - [`switchData`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FawsCLI.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FawsCLI.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FawsCLI.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A15%2C%22character%22%3A2%7D%5D "src/components/awsCLI.tsx"): Function to switch data, takes a boolean and a number as arguments.

#### Component: [`AwsCLI`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FawsCLI.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FawsCLI.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FawsCLI.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A18%2C%22character%22%3A0%7D%5D "src/components/awsCLI.tsx")
- **Props**: Accepts [`AwsCLIProps`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FawsCLI.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FawsCLI.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FawsCLI.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A11%2C%22character%22%3A0%7D%5D "src/components/awsCLI.tsx").
- **State Variables**:
  - [`copied`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FawsCLI.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FawsCLI.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FawsCLI.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A19%2C%22character%22%3A2%7D%5D "src/components/awsCLI.tsx"): Boolean state to track if the text has been copied.
  - [`stackEnable`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FawsCLI.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FawsCLI.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FawsCLI.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A20%2C%22character%22%3A2%7D%5D "src/components/awsCLI.tsx"): Boolean state to track if the stack is enabled.
  - [`templateUrl`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FawsCLI.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FawsCLI.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FawsCLI.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A21%2C%22character%22%3A2%7D%5D "src/components/awsCLI.tsx"): String state to store the template URL.
  - [`accountId`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FawsCLI.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FawsCLI.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FawsCLI.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A22%2C%22character%22%3A2%7D%5D "src/components/awsCLI.tsx"): String state to store the account ID.

#### Hooks
- **[`useEffect`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FawsCLI.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FawsCLI.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FawsCLI.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A1%2C%22character%22%3A0%7D%5D "src/components/awsCLI.tsx")**: Runs once after the component mounts.
  - **Pseudo Logic**:
    1. Fetch configuration using [`getConfiguration`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fapis%2FloginAPI.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A3%2C%22character%22%3A0%7D%5D "src/apis/loginAPI.tsx").
    2. Set [`templateUrl`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FawsCLI.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FawsCLI.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FawsCLI.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A21%2C%22character%22%3A2%7D%5D "src/components/awsCLI.tsx") and [`accountId`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FawsCLI.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FawsCLI.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FawsCLI.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A22%2C%22character%22%3A2%7D%5D "src/components/awsCLI.tsx") from the fetched configuration.

#### Methods
- **[`handleCopy`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FawsCLI.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FawsCLI.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FawsCLI.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A32%2C%22character%22%3A2%7D%5D "src/components/awsCLI.tsx")**: Handles the copy action.
  - **Pseudo Logic**:
    1. Set [`copied`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FawsCLI.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FawsCLI.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FawsCLI.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A19%2C%22character%22%3A2%7D%5D "src/components/awsCLI.tsx") state to `true`.
    2. Call the [`onCopy`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FawsCLI.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FawsCLI.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FawsCLI.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A12%2C%22character%22%3A2%7D%5D "src/components/awsCLI.tsx") callback if provided.

#### Additional Variables
- **[`currentDate`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FawsCLI.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FawsCLI.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FawsCLI.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A37%2C%22character%22%3A2%7D%5D "src/components/awsCLI.tsx")**: Stores the current date.
- **Date Components**:
  - [`year`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FawsCLI.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FawsCLI.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FawsCLI.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A40%2C%22character%22%3A2%7D%5D "src/components/awsCLI.tsx"): Extracts the year from [`currentDate`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FawsCLI.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FawsCLI.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FawsCLI.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A37%2C%22character%22%3A2%7D%5D "src/components/awsCLI.tsx").

### Pseudo Logic for [`AwsCLI`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FawsCLI.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FawsCLI.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FawsCLI.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A18%2C%22character%22%3A0%7D%5D "src/components/awsCLI.tsx") Component
1. **Initialize State**:
   - [`copied`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FawsCLI.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FawsCLI.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FawsCLI.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A19%2C%22character%22%3A2%7D%5D "src/components/awsCLI.tsx") to `false`.
   - [`stackEnable`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FawsCLI.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FawsCLI.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FawsCLI.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A20%2C%22character%22%3A2%7D%5D "src/components/awsCLI.tsx") to `false`.
   - [`templateUrl`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FawsCLI.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FawsCLI.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FawsCLI.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A21%2C%22character%22%3A2%7D%5D "src/components/awsCLI.tsx") to an empty string.
   - [`accountId`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FawsCLI.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FawsCLI.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FawsCLI.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A22%2C%22character%22%3A2%7D%5D "src/components/awsCLI.tsx") to an empty string.

2. **Fetch Configuration on Mount**:
   - Use [`useEffect`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FawsCLI.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FawsCLI.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FawsCLI.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A1%2C%22character%22%3A0%7D%5D "src/components/awsCLI.tsx") to fetch configuration data.
   - Set [`templateUrl`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FawsCLI.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FawsCLI.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FawsCLI.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A21%2C%22character%22%3A2%7D%5D "src/components/awsCLI.tsx") and [`accountId`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FawsCLI.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FawsCLI.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FawsCLI.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A22%2C%22character%22%3A2%7D%5D "src/components/awsCLI.tsx") based on the fetched configuration.

3. **Handle Copy Action**:
   - Define [`handleCopy`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FawsCLI.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FawsCLI.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FawsCLI.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A32%2C%22character%22%3A2%7D%5D "src/components/awsCLI.tsx") method to update [`copied`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FawsCLI.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FawsCLI.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FawsCLI.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A19%2C%22character%22%3A2%7D%5D "src/components/awsCLI.tsx") state and trigger [`onCopy`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FawsCLI.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FawsCLI.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FawsCLI.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A12%2C%22character%22%3A2%7D%5D "src/components/awsCLI.tsx") callback.

4. **Current Date**:
   - Store the current date in [`currentDate`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FawsCLI.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FawsCLI.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FawsCLI.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A37%2C%22character%22%3A2%7D%5D "src/components/awsCLI.tsx").
   - Extract and store the year from [`currentDate`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FawsCLI.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FawsCLI.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FawsCLI.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A37%2C%22character%22%3A2%7D%5D "src/components/awsCLI.tsx").

### Code
```tsx
import { unicode } from "@fortawesome/free-regular-svg-icons/faAddressBook";
import React, { useEffect, useState } from "react";
import { CopyToClipboard } from "react-copy-to-clipboard";
import { createAccountStackCheck } from "../apis/createAccountCheckStack";
import { create } from "@mui/material/styles/createTransitions";
import { useNavigate } from "react-router-dom";
import { getRegistedUser } from "../services/authService";
import { User } from "../utils/interfaces";
import { createStackFuncAPI } from "../apis/createStackAPI";
import { getConfiguration } from "../apis/loginAPI";

interface AwsCLIProps {
  onCopy?: () => void;
  uniqueId?: string;
  externalId?: string;
  switchData: (boolData: boolean, swithNumber: number) => void;
}

const AwsCLI: React.FC<AwsCLIProps> = (props, { onCopy }) => {
  const [copied, setCopied] = useState(false);
  const [stackEnable, setStackEnable] = useState(false);
  const [templateUrl, setTemplateUrl] = useState("");
  const [accountId, setAccountID] = useState("");

  useEffect(() => {
    void (async () => {
      const configuration = await getConfiguration();
      setTemplateUrl(configuration.stack.aws_console_template_url);
      setAccountID(configuration.stack.aws_source_account_id);
    })();
  }, []);

  const handleCopy = () => {
    setCopied(true);
    onCopy?.();
  };

  const currentDate = new Date();
  const year = currentDate.getFullYear();
};
```

This documentation provides a detailed overview of the `awsCLI.tsx` file, including its imports, interfaces, component structure, state management, hooks, and methods.

sumitchohan: generate detailed documentation for current code file including all methods, fields, properties and summary of their implementation including pseudo logic

GitHub Copilot: ### Documentation for [`azureAccountList.tsx`](command:_github.copilot.openRelativePath?%5B%7B%22scheme%22%3A%22file%22%2C%22authority%22%3A%22%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FazureAccountList.tsx%22%2C%22query%22%3A%22%22%2C%22fragment%22%3A%22%22%7D%5D "/Users/sumit/Downloads/Frontend/NAGISVPR/src/components/azureAccountList.tsx")

#### Summary
This file defines a React functional component named [`AzureAccountList`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FazureAccountList.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FazureAccountList.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FazureAccountList.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A11%2C%22character%22%3A0%7D%5D "src/components/azureAccountList.tsx") which is responsible for rendering a list of Azure accounts in a table format. The component receives several props to manage the state and behavior of the account list.

#### Interfaces

##### [`AzureAccountListProps`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FazureAccountList.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FazureAccountList.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FazureAccountList.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A3%2C%22character%22%3A0%7D%5D "src/components/azureAccountList.tsx")
This interface defines the properties that the [`AzureAccountList`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FazureAccountList.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FazureAccountList.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FazureAccountList.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A11%2C%22character%22%3A0%7D%5D "src/components/azureAccountList.tsx") component expects to receive as props.

- **`accounts: Account[]`**: An array of [`Account`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Futils%2Finterfaces.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A81%2C%22character%22%3A0%7D%5D "src/utils/interfaces.tsx") objects representing the Azure accounts to be displayed.
- **`setRegionSidebarOpen: (open: boolean) => void`**: A function to control the visibility of the region sidebar.
- **`getAccount: (account: Account) => void`**: A function to retrieve details of a specific account.
- **`setCloudProvider: (value: string) => void`**: A function to set the cloud provider.
- **`setAccountAlias: (value: string) => void`**: A function to set the alias for an account.

#### Component

##### [`AzureAccountList`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FazureAccountList.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FazureAccountList.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FazureAccountList.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A11%2C%22character%22%3A0%7D%5D "src/components/azureAccountList.tsx")
This is a React functional component that takes [`AzureAccountListProps`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FazureAccountList.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FazureAccountList.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FazureAccountList.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A3%2C%22character%22%3A0%7D%5D "src/components/azureAccountList.tsx") as its props and returns a JSX element. The component renders a table with headers for displaying Azure account information.

###### Props
- **[`accounts`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FazureAccountList.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FazureAccountList.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FazureAccountList.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A4%2C%22character%22%3A2%7D%5D "src/components/azureAccountList.tsx")**: An array of [`Account`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Futils%2Finterfaces.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A81%2C%22character%22%3A0%7D%5D "src/utils/interfaces.tsx") objects.
- **[`setRegionSidebarOpen`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FazureAccountList.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FazureAccountList.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FazureAccountList.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A5%2C%22character%22%3A2%7D%5D "src/components/azureAccountList.tsx")**: Function to open/close the region sidebar.
- **[`getAccount`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FazureAccountList.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FazureAccountList.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FazureAccountList.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A6%2C%22character%22%3A2%7D%5D "src/components/azureAccountList.tsx")**: Function to get account details.
- **[`setCloudProvider`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FazureAccountList.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FazureAccountList.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FazureAccountList.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A7%2C%22character%22%3A2%7D%5D "src/components/azureAccountList.tsx")**: Function to set the cloud provider.
- **[`setAccountAlias`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FazureAccountList.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FazureAccountList.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FazureAccountList.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A8%2C%22character%22%3A2%7D%5D "src/components/azureAccountList.tsx")**: Function to set the account alias.

###### Pseudo Logic
1. **Render a `div` element**: This acts as a container for the table.
2. **Render a `table` element**: This table will display the Azure account information.
3. **Render `thead` element**: This contains the headers for the table.
   - **Render `td` elements**: These are the table headers for "Subscription Alias", "Subscription ID", and "Created on".
   - **Commented out `td` element**: There is a commented-out header for "Total Regions".

#### Code
```tsx
import React from "react";
import { Account } from "../utils/interfaces";

interface AzureAccountListProps {
  accounts: Account[];
  setRegionSidebarOpen: (open: boolean) => void;
  getAccount: (account: Account) => void;
  setCloudProvider: (value: string) => void;
  setAccountAlias: (value: string) => void;
}

const AzureAccountList: React.FC<AzureAccountListProps> = (props) => {
  return (
    <div>
      {/* Render the list of Azure accounts here */}
      <table className="w-full">
        <thead>
          <td className="p-4 text-sm font-medium break-words border border-[#E6E6E6] w-1/3">
            Subscription Alias
          </td>
          <td className="p-4 text-sm font-medium break-words border border-[#E6E6E6]  w-1/3">
            Subscription ID
          </td>

          {/* <td className="p-4 text-sm font-medium break-words border border-[#E6E6E6]  w-1/12">
            Total Regions
          </td> */}
          <td className="p-4 text-sm font-medium break-words border border-[#E6E6E6] w-1/3">
            Created on
          </td>
        </thead>
      </table>
    </div>
  );
};

export default AzureAccountList;
```

This documentation provides a detailed overview of the `azureAccountList.tsx` file, including the purpose of the component, the props it receives, and the structure of the rendered JSX.

sumitchohan: generate detailed documentation for current code file including all methods, fields, properties and summary of their implementation including pseudo logic



GitHub Copilot: ### azureAccountSetup.tsx Documentation

#### Summary
This file is a React component that sets up the Azure account configuration interface. It imports various components and utilities required for the setup process. The file defines two main components, [`Current`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FazureAccountSetup.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FazureAccountSetup.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FazureAccountSetup.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A23%2C%22character%22%3A0%7D%5D "src/components/azureAccountSetup.tsx") and [`Next`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FazureAccountSetup.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FazureAccountSetup.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FazureAccountSetup.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A33%2C%22character%22%3A0%7D%5D "src/components/azureAccountSetup.tsx"), which are used to display the current and next steps in the setup process.

#### Imports
- **React, useState**: Core React library and hook for managing state.
- **AccountSetupForm**: Component for the account setup form.
- **AwsCLI**: Component for AWS CLI setup.
- **SelectRegion, EnviromentAlias, SelectMethod**: Components for AWS form elements.
- **Navigate, useNavigate**: React Router components for navigation.
- **SelectCloud**: Component for selecting the cloud provider.
- **AwsConsole, AccountAlias**: Additional AWS form components.
- **AzureAccountAlias, AzureSelectRegions, AzureSelectMethod, AzureCli**: Components for Azure form elements.

#### Interfaces
- **AzureAccountSetupProps**
  - **onNext**: Function to handle the next step.
  - **onPrev**: Function to handle the previous step.
- **SelectedProps**
  - **number**: String representing the step number.

#### Components

1. **Current**
   - **Props**: [`SelectedProps`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FazureAccountSetup.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FazureAccountSetup.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FazureAccountSetup.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A20%2C%22character%22%3A0%7D%5D "src/components/azureAccountSetup.tsx")
   - **Description**: Displays the current step in the setup process.
   - **Pseudo Logic**:
     - Render a `div` with specific styling.
     - Inside the `div`, render another `div` with the step number.

2. **Next**
   - **Props**: [`SelectedProps`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FazureAccountSetup.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FazureAccountSetup.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FazureAccountSetup.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A20%2C%22character%22%3A0%7D%5D "src/components/azureAccountSetup.tsx")
   - **Description**: Displays the next step in the setup process.
   - **Pseudo Logic**:
     - Render a `div` with specific styling.
     - Inside the `div`, render another `div` with the step number.

#### Pseudo Logic for Components

**Current Component:**
1. Create a `div` with classes for flexbox, centering, height, width, border, background color, and rounded corners.
2. Inside this `div`, create another `div` with classes for background color, text color, text size, font weight, height, width, rounded corners, and flexbox centering.
3. Display the [`number`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FazureAccountSetup.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FazureAccountSetup.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FazureAccountSetup.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A21%2C%22character%22%3A2%7D%5D "src/components/azureAccountSetup.tsx") prop inside the inner `div`.

**Next Component:**
1. Create a `div` with classes for flexbox, centering, height, width, border, background color, and rounded corners.
2. Inside this `div`, create another `div` with classes for background color, text color, text size, font weight, height, width, rounded corners, and flexbox centering.
3. Display the [`number`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FazureAccountSetup.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FazureAccountSetup.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FazureAccountSetup.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A21%2C%22character%22%3A2%7D%5D "src/components/azureAccountSetup.tsx") prop inside the inner `div`.

### Example Usage
```tsx
<Current number="1" />
<Next number="2" />
```

This documentation provides a detailed overview of the `azureAccountSetup.tsx` file, including its components, props, and pseudo logic for rendering the `Current` and `Next` components.

sumitchohan: generate detailed documentation for current code file including all methods, fields, properties and summary of their implementation including pseudo logic

GitHub Copilot: ### Documentation for [`complianceStatusCard.tsx`](command:_github.copilot.openRelativePath?%5B%7B%22scheme%22%3A%22file%22%2C%22authority%22%3A%22%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FcomplianceStatusCard.tsx%22%2C%22query%22%3A%22%22%2C%22fragment%22%3A%22%22%7D%5D "/Users/sumit/Downloads/Frontend/NAGISVPR/src/components/complianceStatusCard.tsx")

#### Summary
This file defines a React functional component [`ComplianceStatusCard`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FcomplianceStatusCard.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FcomplianceStatusCard.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FcomplianceStatusCard.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A29%2C%22character%22%3A0%7D%5D "src/components/complianceStatusCard.tsx") that displays a pie chart representing policy compliance status. It uses the `recharts` library for rendering the pie chart and [`Tippy`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FcomplianceStatusCard.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FcomplianceStatusCard.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FcomplianceStatusCard.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A3%2C%22character%22%3A0%7D%5D "src/components/complianceStatusCard.tsx") for tooltips. The component fetches compliance data and allows toggling between deployed and total states.

#### Imports
- **React**: Core library for building the component.
- **useEffect, useState**: React hooks for managing side effects and state.
- **PieChart, Pie, Cell**: Components from `recharts` for rendering pie charts.
- **icon**: An SVG icon for tooltips.
- **Tippy**: A library for tooltips.
- **PolicyCompStatus**: Interface for policy compliance status.
- **getPolicyComplianceStatus**: API function to fetch compliance status.
- **ExceptionListSideBar**: Another component, presumably for listing exceptions.

#### Interfaces
- **DataItem**
  - [`name`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FcomplianceStatusCard.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FcomplianceStatusCard.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FcomplianceStatusCard.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A11%2C%22character%22%3A2%7D%5D "src/components/complianceStatusCard.tsx"): `string` - The name of the data item.
  - [`value`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FcomplianceStatusCard.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FcomplianceStatusCard.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FcomplianceStatusCard.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A12%2C%22character%22%3A2%7D%5D "src/components/complianceStatusCard.tsx"): `number` - The value associated with the data item.

- **ComplianceStatusCard**
  - [`data`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FcomplianceStatusCard.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FcomplianceStatusCard.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FcomplianceStatusCard.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A30%2C%22character%22%3A2%7D%5D "src/components/complianceStatusCard.tsx"): [`PolicyCompStatus`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Futils%2Finterfaces.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A30%2C%22character%22%3A0%7D%5D "src/utils/interfaces.tsx") - The compliance status data passed as a prop.

#### Constants
- **COLORS_DEPLOYED**: Array of colors for the deployed state pie chart.
- **COLORS_TOTAL**: Array of colors for the total state pie chart.
- **dataDeployed**: Initial data for the deployed state pie chart.
  - `Compliant`: Initially set to 0.
  - `Non-Compliant`: Initially set to 0.
- **dataTotal**: Initial data for the total state pie chart.
  - `Deployed`: Initially set to 0.
  - `Not Deployed`: Initially set to 0.

#### Component: [`ComplianceStatusCard`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FcomplianceStatusCard.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FcomplianceStatusCard.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FcomplianceStatusCard.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A29%2C%22character%22%3A0%7D%5D "src/components/complianceStatusCard.tsx")
- **Props**
  - [`data`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FcomplianceStatusCard.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FcomplianceStatusCard.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FcomplianceStatusCard.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A30%2C%22character%22%3A2%7D%5D "src/components/complianceStatusCard.tsx"): [`PolicyCompStatus`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Futils%2Finterfaces.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A30%2C%22character%22%3A0%7D%5D "src/utils/interfaces.tsx") - The compliance status data.

- **State**
  - [`isDeployedSelected`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FcomplianceStatusCard.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FcomplianceStatusCard.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FcomplianceStatusCard.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A34%2C%22character%22%3A2%7D%5D "src/components/complianceStatusCard.tsx"): `boolean` - Indicates whether the deployed state is selected. Initialized to `true`.
  - [`currentData`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FcomplianceStatusCard.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FcomplianceStatusCard.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FcomplianceStatusCard.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A35%2C%22character%22%3A2%7D%5D "src/components/complianceStatusCard.tsx"): [`DataItem[]`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FcomplianceStatusCard.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FcomplianceStatusCard.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FcomplianceStatusCard.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A10%2C%22character%22%3A0%7D%5D "src/components/complianceStatusCard.tsx") - The data to be displayed in the pie chart. Initialized based on [`isDeployedSelected`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FcomplianceStatusCard.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FcomplianceStatusCard.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FcomplianceStatusCard.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A34%2C%22character%22%3A2%7D%5D "src/components/complianceStatusCard.tsx").

- **Logic**
  - **Initialization**
    - [`isDeployedSelected`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FcomplianceStatusCard.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FcomplianceStatusCard.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FcomplianceStatusCard.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A34%2C%22character%22%3A2%7D%5D "src/components/complianceStatusCard.tsx") is initialized to `true`.
    - [`currentData`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FcomplianceStatusCard.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FcomplianceStatusCard.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FcomplianceStatusCard.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A35%2C%22character%22%3A2%7D%5D "src/components/complianceStatusCard.tsx") is initialized to [`dataDeployed`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FcomplianceStatusCard.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FcomplianceStatusCard.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FcomplianceStatusCard.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A20%2C%22character%22%3A0%7D%5D "src/components/complianceStatusCard.tsx") if [`isDeployedSelected`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FcomplianceStatusCard.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FcomplianceStatusCard.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FcomplianceStatusCard.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A34%2C%22character%22%3A2%7D%5D "src/components/complianceStatusCard.tsx") is `true`, otherwise to [`dataTotal`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FcomplianceStatusCard.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FcomplianceStatusCard.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FcomplianceStatusCard.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A24%2C%22character%22%3A0%7D%5D "src/components/complianceStatusCard.tsx").
    - [`currentColors`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FcomplianceStatusCard.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FcomplianceStatusCard.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FcomplianceStatusCard.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A39%2C%22character%22%3A2%7D%5D "src/components/complianceStatusCard.tsx") is set based on [`isDeployedSelected`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FcomplianceStatusCard.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FcomplianceStatusCard.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FcomplianceStatusCard.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A34%2C%22character%22%3A2%7D%5D "src/components/complianceStatusCard.tsx").

  - **Rendering**
    - The component renders a pie chart using [`PieChart`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FcomplianceStatusCard.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FcomplianceStatusCard.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FcomplianceStatusCard.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A1%2C%22character%22%3A0%7D%5D "src/components/complianceStatusCard.tsx"), [`Pie`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FcomplianceStatusCard.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FcomplianceStatusCard.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FcomplianceStatusCard.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A1%2C%22character%22%3A0%7D%5D "src/components/complianceStatusCard.tsx"), and [`Cell`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FcomplianceStatusCard.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FcomplianceStatusCard.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FcomplianceStatusCard.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A1%2C%22character%22%3A0%7D%5D "src/components/complianceStatusCard.tsx") from `recharts`.
    - Tooltips are provided using [`Tippy`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FcomplianceStatusCard.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FcomplianceStatusCard.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FcomplianceStatusCard.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A3%2C%22character%22%3A0%7D%5D "src/components/complianceStatusCard.tsx").

#### Pseudo Logic
1. **Initialize State**
   - [`isDeployedSelected`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FcomplianceStatusCard.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FcomplianceStatusCard.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FcomplianceStatusCard.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A34%2C%22character%22%3A2%7D%5D "src/components/complianceStatusCard.tsx") is set to `true`.
   - [`currentData`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FcomplianceStatusCard.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FcomplianceStatusCard.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FcomplianceStatusCard.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A35%2C%22character%22%3A2%7D%5D "src/components/complianceStatusCard.tsx") is set to [`dataDeployed`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FcomplianceStatusCard.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FcomplianceStatusCard.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FcomplianceStatusCard.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A20%2C%22character%22%3A0%7D%5D "src/components/complianceStatusCard.tsx") if [`isDeployedSelected`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FcomplianceStatusCard.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FcomplianceStatusCard.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FcomplianceStatusCard.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A34%2C%22character%22%3A2%7D%5D "src/components/complianceStatusCard.tsx") is `true`, otherwise to [`dataTotal`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FcomplianceStatusCard.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FcomplianceStatusCard.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FcomplianceStatusCard.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A24%2C%22character%22%3A0%7D%5D "src/components/complianceStatusCard.tsx").
   - [`currentColors`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FcomplianceStatusCard.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FcomplianceStatusCard.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FcomplianceStatusCard.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A39%2C%22character%22%3A2%7D%5D "src/components/complianceStatusCard.tsx") is set based on [`isDeployedSelected`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FcomplianceStatusCard.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FcomplianceStatusCard.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FcomplianceStatusCard.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A34%2C%22character%22%3A2%7D%5D "src/components/complianceStatusCard.tsx").

2. **Render Pie Chart**
   - Use [`PieChart`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FcomplianceStatusCard.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FcomplianceStatusCard.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FcomplianceStatusCard.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A1%2C%22character%22%3A0%7D%5D "src/components/complianceStatusCard.tsx") to create a pie chart.
   - Use [`Pie`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FcomplianceStatusCard.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FcomplianceStatusCard.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FcomplianceStatusCard.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A1%2C%22character%22%3A0%7D%5D "src/components/complianceStatusCard.tsx") to define the pie slices.
   - Use [`Cell`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FcomplianceStatusCard.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FcomplianceStatusCard.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FcomplianceStatusCard.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A1%2C%22character%22%3A0%7D%5D "src/components/complianceStatusCard.tsx") to color the slices based on [`currentColors`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FcomplianceStatusCard.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FcomplianceStatusCard.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FcomplianceStatusCard.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A39%2C%22character%22%3A2%7D%5D "src/components/complianceStatusCard.tsx").

3. **Tooltips**
   - Use [`Tippy`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FcomplianceStatusCard.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FcomplianceStatusCard.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FcomplianceStatusCard.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A3%2C%22character%22%3A0%7D%5D "src/components/complianceStatusCard.tsx") to add tooltips to the pie chart.

4. **Toggle State**
   - Provide a mechanism to toggle between deployed and total states, updating [`isDeployedSelected`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FcomplianceStatusCard.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FcomplianceStatusCard.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FcomplianceStatusCard.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A34%2C%22character%22%3A2%7D%5D "src/components/complianceStatusCard.tsx") and [`currentData`](command:_github.copilot.openSymbolFromReferences?%5B%7B%22%24mid%22%3A1%2C%22fsPath%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FcomplianceStatusCard.tsx%22%2C%22external%22%3A%22file%3A%2F%2F%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FcomplianceStatusCard.tsx%22%2C%22path%22%3A%22%2FUsers%2Fsumit%2FDownloads%2FFrontend%2FNAGISVPR%2Fsrc%2Fcomponents%2FcomplianceStatusCard.tsx%22%2C%22scheme%22%3A%22file%22%7D%2C%7B%22line%22%3A35%2C%22character%22%3A2%7D%5D "src/components/complianceStatusCard.tsx") accordingly.

#### Code
```tsx
import React, { useEffect, useState } from "react";
import { PieChart, Pie, Cell } from "recharts";
import icon from "../assets/icons/icon-tool-tip.svg";
import Tippy from "@tippyjs/react";
import "tippy.js/dist/tippy.css";
import { PolicyCompStatus } from "../utils/interfaces";
import { getPolicyComplianceStatus } from "../apis/dasboardAPI";
import ExceptionListSideBar from "./exceptionListSideBar";

// Define types for your data array
interface DataItem {
  name: string;
  value: number;
}

// Colors for the Deployed and Total states
const COLORS_DEPLOYED: string[] = ["#3AAB94", "#F8485E"];
const COLORS_TOTAL: string[] = ["#4E5E78", "#E0E3E8"];

// Data for Deployed and Total states
const dataDeployed: DataItem[] = [
  { name: "Compliant", value: 0 },
  { name: "Non-Compliant", value: 0 },
];
const dataTotal: DataItem[] = [
  { name: "Deployed", value: 0 },
  { name: "Not Deployed", value: 0 },
];

interface ComplianceStatusCard {
  data: PolicyCompStatus;
}

const ComplianceStatusCard: React.FC<ComplianceStatusCard> = (props) => {
  const [isDeployedSelected, setIsDeployedSelected] = useState<boolean>(true);
  const [currentData, setCurrentData] = useState<DataItem[]>(
    isDeployedSelected ? dataDeployed : dataTotal
  );
  const currentColors = isDeployedSelected ? COLORS_DEPLOYED : COLORS_TOTAL;

  // Render logic here
};
```

This documentation provides a detailed overview of the `complianceStatusCard.tsx` file, including its purpose, structure, and logic.

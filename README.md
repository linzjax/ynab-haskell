# ynab-haskell

**Work in progress**: This is currently just a pet project, so I make no
promises about maintaining it or functionality.

## Getting your YNAB API Token
Head on over to YNAB's [Personal Access Tokens](https://api.youneedabudget.com/#personal-access-tokens) page for
instructions on generating a token.

## Current Progress

### To Do

### In Progress
- Implement functionality for all API endpoints
- Add Tests
- Rethink naming format

### Done


## Endpoint Progress
Attempting to implement functionality for all endpoints listed here, as of 2018-09-06.

#### User
- [x] [GET] /user - getUser

#### Budgets
- [x] [GET] /budgets 
   - `getBudgets`
- [x] [GET] /budgets/{budget_id} 
   - `getBudgetById (budgetId :: Text)`
- [x] [GET] /budgets/{budget_id}/settings 
   - `getBudgetSettingsById (budgetId :: Text)`

#### Accounts
The Accounts for a budget.

- [x] [GET] /budgets/{budget_id}/accounts 
   - `getAccounts (budgetId :: Text)`
- [x] [GET] /budgets/{budget_id}/accounts/{account_id} 
   - `getAccountById (budgetId :: Text) (accountId :: Text)`

#### Categories
The Categories for a budget.

- [x] [GET] /budgets/{budget_id}/categories 
   - `getCategories (budgetId :: Text)`
- [x] [GET] /budgets/{budget_id}/categories/{category_id} 
   - `getCategoriesById (budgetId :: Text) (categoryId :: Text)`

#### Payees
The Payees for a budget.

- [x] [GET] /budgets/{budget_id}/payees 
   - `getPayees (budgetId :: Text)`
- [x] [GET] /budgets/{budget_id}/payees/{payee_id} 
   - `getPayeeById (budgetId :: Text) (payeeId :: Text)`

#### Payee Locations
When you enter a transaction and specify a payee on the YNAB mobile apps, the GPS coordinates for that location are stored, with your permission, so that the next time you are in the same place (like the Grocery store) we can pre-populate nearby payees for you! It’s handy and saves you time. This resource makes these locations available. Locations will not be available for all payees.

- [x] [GET] /budgets/{budget_id}/payee_locations 
   - `getPayeeLocations (budgetId :: Text)`

> Note about these two: since I don't use payee locations, I currently have no idea if these end points work. They should work in theory.

- [x] [GET] /budgets/{budget_id}/payee_locations/{payee_location_id} 
   - `getPayeeLocationById (budgetId :: Text) (plId :: Text)`
- [x] [GET] /budgets/{budget_id}/payees/{payee_id}/payee_locations 
   - `getPayeeLocations (budgetId :: Text) (payeeId :: Text)`

#### Months
Each budget contains one or more months, which is where To be Budgeted, Age of Money and Category (budgeted / activity / balances) amounts are available.

- [x] [GET] /budgets/{budget_id}/months 
   - `getBudgetMonths (budgetId :: Text)`
- [x] [GET] /budgets/{budget_id}/months/{month} 
   - `getBudgetMonth (budgetId :: Text) (monthstring :: Text (i.e. "2018-09-01" or "current"))`

#### Transactions
The Transactions for a budget.

- [x] [GET] /budgets/{budget_id}/transactions
   - `getTransactions (budgetId :: Text)`
- [x] [POST] /budgets/{budget_id}/transactions      
  - `postTransaction (budgetId :: Text) (SaveTransactionWrapper (transaction :: SaveTransaction))`
  - `postTransactions (budgetId :: Text) (SaveTransactionsWrapper (transactions :: [SaveTransaction]))`
- [ ] [POST] /budgets/{budget_id}/transactions/bulk
 
- [x] [GET] /budgets/{budget_id}/accounts/{account_id}/transactions
   - `getTransactionsByAccountId (budgetId :: Text) (accountId :: Text)`
- [x] [GET] /budgets/{budget_id}/categories/{category_id}/transactions
   - `getTransactionsByCategoryId (budgetId :: Text) (categoryId :: Text)`
- [x] [GET] /budgets/{budget_id}/payees/{payee_id}/transactions
   - `getTransactionsByPayeeId (budgetId :: Text) (payeeId :: Text)`
- [x] [GET] /budgets/{budget_id}/transactions/{transaction_id}
   - `getTransactionsByTransactionId (budgetId :: Text) (transactionId :: Text)`
- [ ] [PUT] /budgets/{budget_id}/transactions/{transaction_id}

#### Scheduled Transactions
The Scheduled Transactions for a budget.

- [x] [GET] /budgets/{budget_id}/scheduled_transactions
   - `getScheduledTransactions (budgetId :: Text)`
- [x] [GET] /budgets/{budget_id}/scheduled_transactions/{scheduled_transaction_id}
   - `getScheduledTransactionById (budgetId :: Text) (schedulTransactionId :: Text)`
---
Shoutout to [ConnorGriffin](https://github.com/ConnorGriffin/) for his [README.md](https://github.com/ConnorGriffin/Posh-YNAB) format inspiration.

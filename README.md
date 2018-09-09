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
- [x] [GET] /budgets - getBudgets
- [x] [GET] /budgets/{budget_id} - getBudgetById (budgetId :: Text)
- [x] [GET] /budgets/{budget_id}/settings - getBudgetSettingsById (budgetId :: Text)

#### Accounts
The Accounts for a budget.

- [x] [GET] /budgets/{budget_id}/accounts - getAccounts (budgetId :: Text)
- [x] [GET] /budgets/{budget_id}/accounts/{account_id} - getAccountById (budgetId :: Text) (accountId :: Text)

#### Categories
The Categories for a budget.

- [x] [GET] /budgets/{budget_id}/categories - getCategories (budgetId :: Text)
- [x] [GET] /budgets/{budget_id}/categories/{category_id} - getCategoriesById (budgetId :: Text) (categoryId :: Text)

#### Payees
The Payees for a budget.

- [ ] [GET] /budgets/{budget_id}/payees
- [ ] [GET] /budgets/{budget_id}/payees/{payee_id}

#### Payee Locations
When you enter a transaction and specify a payee on the YNAB mobile apps, the GPS coordinates for that location are stored, with your permission, so that the next time you are in the same place (like the Grocery store) we can pre-populate nearby payees for you! It’s handy and saves you time. This resource makes these locations available. Locations will not be available for all payees.

- [ ] [GET] /budgets/{budget_id}/payee_locations
- [ ] [GET] /budgets/{budget_id}/payee_locations/{payee_location_id}
- [ ] [GET] /budgets/{budget_id}/payees/{payee_id}/payee_locations

#### Months
Each budget contains one or more months, which is where To be Budgeted, Age of Money and Category (budgeted / activity / balances) amounts are available.

- [ ] [GET] /budgets/{budget_id}/months
- [ ] [GET] /budgets/{budget_id}/months/{month}

#### Transactions
The Transactions for a budget.

- [ ] [GET] /budgets/{budget_id}/transactions
- [ ] [POST] /budgets/{budget_id}/transactions
- [ ] [POST] /budgets/{budget_id}/transactions/bulk
- [ ] [GET] /budgets/{budget_id}/accounts/{account_id}/transactions
- [ ] [GET] /budgets/{budget_id}/categories/{category_id}/transactions
- [ ] [GET] /budgets/{budget_id}/payees/{payee_id}/transactions
- [ ] [GET] /budgets/{budget_id}/transactions/{transaction_id}
- [ ] [PUT] /budgets/{budget_id}/transactions/{transaction_id}

#### Scheduled Transactions
The Scheduled Transactions for a budget.

- [ ] [GET] /budgets/{budget_id}/scheduled_transactions
- [ ] [GET] /budgets/{budget_id}/scheduled_transactions/{scheduled_transaction_id}

---
Shoutout to [ConnorGriffin](https://github.com/ConnorGriffin/) for his [README.md](https://github.com/ConnorGriffin/Posh-YNAB) format inspiration.

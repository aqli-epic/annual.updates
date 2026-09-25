# AQLI Analytics Explorer

Current AQLI analytics dashboard. The earlier explorer is `apps/data-explorer/`. The AQ Fund app is `apps/aqfund/`.

Restore packages from this folder's `renv.lock` (R 4.3.3) with `renv::restore()`.

## Login

The app reads credentials from the environment. Set both variables to comma-separated lists of the same length:

```bash
export AQLI_DASHBOARD_USERS="analyst"
export AQLI_DASHBOARD_PASSWORDS="replace-with-a-new-password"
```

Passwords that were previously committed in `global.R` are no longer in the file. Rotate those passwords before deploying again. Do not commit the new values, and do not commit `rsconnect/` records.

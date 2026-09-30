---
name: Workflow process lifecycle
description: Stale Zig server processes that can outlive a failed workflow restart.
---

A Zig `Start application` workflow can report a restart failure from `AddressInUse` while an older compiled server process remains alive, even with its executable marked deleted. In that state, the preview/API may still be served by the older process while the workflow itself is failed.

**Why:** The previous process was not visibly tied to the workflow status; treating the error as an application compile failure would have led to unnecessary code changes.

**How to apply:** When a restart fails with `AddressInUse`, identify the listener and check whether the app is still responding before changing source or retrying. Stop only the confirmed stale server process, then restart the configured workflow and verify its open port and API.
# nm-cli CLI Client for Notenmanagement of HTL-Braunau
- teacher and student login
- supports only viewing data like absences/grades/assessments

## building
`build.sh --target<platform>` currently supports x86_64-linux and x86_64-macosx

creates folder containing bundled release that has platform specific executable under /bin and racket runtime + dependencies bundled under /lib

uses raco-cross for platform compilation

building requires Racket >v8.11.1

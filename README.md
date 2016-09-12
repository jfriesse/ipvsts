### Overview
ipvsts (The IP Virtual Server Testing Suite) was testing suite for The IP Virtual Server.
Currently it's unmaintained and it's moved to GitHub for archive reasons because fedorahosted is planned for shutdown.
It had some unique features (biggest one was implementation language Scheme (Guile 1.x)).

### Features
 * Installation of RHEL 6 via automatically generated kickstart
 * Update installation
 * Start VMs with specific network configuration
 * Test ipvsadm
  * Init script
  * Module load/unload
  * Save / Restore rules
  * Bad parameters handling
  * Correct interaction between ipvsadm and kernel ipvs module

### Usage
#### Source code
For stable version, use https://github.com/jfriesse/ipvsts/releases/download/20110317/ipvsts-20110317.tar.gz.
For newest git, use
```
$ git clone git://github.com/jfriesse/ipvsts.git
```
#### Running
Use `doc/examples/test-sl6x-i386.scm` as starting point. This test automatically installs Scientific Linux 6 for i386 VM to `~/vms` and runs ipvsadm test.
### Reporting Bugs
Project is no longer maintained so no bug reporting.
### Who
This project was maintained by Jan Friesse ([Red Hat](http://www.redhat.com))
### Previously planned features
* Logs in sexp version
* Tool for reading such logs and displaying as tree
* Test ipvs 

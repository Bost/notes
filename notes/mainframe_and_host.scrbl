#lang notes

#+title: Mainframe and Host

@block{@block-name{Mainframe and Host}
  # ? Automated testing ?
  OPSS

  # Program Temporary Fix
  PTF

  # double-byte character set
  DBCS

  # spufi: browse commands: columns / reset: toggle column numbers
  col / res

  # spufi: toggle: edit SQL command / view results
  ;;; F3

  # flip screen: toggle projector / display screen mode
  Alt-f

  # sdsf: auto refresh every 10 seconds - does not work/ refresh
  &10 / Enter / Alt-a

  # ispf: jump to menu
  Alt-h

  # batch programs
  COBOL + / standalone / DB2 / VSAM / DB2 + VSAM

  # online programs
  COBOL + / standalone / CICS / DB2 + CICS / VSAM + CICS / DB2 + VSAM + CICS

  # Version Control Tools; like MVS
  Librarion / Changeman / Endevor / Penvalent

  # Debuggers
  XPEDITOR / INTERTEST

  # Diagnostic Tool Framework for Java
  DTFJ

  # sdsf: show extra info about jobs
  ?

  # Record-oriented IO access to VSAM / non VSAM (PDS or sequential) data sets and HFS files. Deprecated - use JZOS
  JRIO

  # Enterprise Resource Planning
  ERP

  # Input Output Processor
  IOP

  # Licensed Internal Code
  LIC

  # System Managed Storage / Storage Management Subsystem
  SMS

  # Control Unit
  CU

  # Processing Unit: brain - executes instructions; inside CPC cage(frame)
  PU

  # Customer Processor: normal processor
  CP

  # Central Processing Unit (is not Processor)
  CPU

  # Central Processing Complex: a cage for Processing Units / CPUs; the box
  CPC

  # Central Processors: general purpose processors
  CPs

  # PUs (speciality engine) for encryption / decription
  CPACF

  # Integrated Facility for Linux: PUs (speciality engine) for Linux Workload
  IFL

  # (Internal) Coupling Facilities
  CFs / ICFs

  # Coupling Facility Control Code
  CFCC

  # speciality engine: Integrated Coupling Facility: uses CFCC and LIC
  ICF

  # Uncharactersed PU functions: replacement for broken CP or system assist processor
  Spare

  # Capacity on Demand: for peak loads
  CoD

  # Prefix Storage Area: small area of memory on each processor; unique to that processor; for instruction execution, interrupts, err handling
  PSA

  # Signal Processor - an instruction; for err recovery
  SIGP

  # Host Adapter
  HA

  # Small Computer System Interface
  SCSI

  # SQL Processing File Input
  SPUFI

  # tso MSV eXtended Information
  tso mxi

  # spufi: start SPUFI from Apps
  tso db2ei

  # Screen Definition Facility
  SDF

  # Storage Subsystem ID
  SSID

  # Control Unit: ?
  CU:LDEV

  # Enterprise Identity Mapping
  EIM

  # Data Facility Storage Management Subsystem - manage data from creation to expiration
  DFSMS

  # Prefixed Save Area: Low Core
  PSA

  # Java Bridge Java Server - special JVM, a.k.a. Library Server JVM, InfoCenter-related Java-based functions
  JBJS

  # editor: highlighting; works for Selection
  hilite jcl / cobol / rexx / off / pascal

  # editor:
  l <lineNumber> / f <searchString>

  # dataset with info about attributes and volumes of other datasets. Less info about storage setup needed (no changes in JCL DD statesments) when a catalogized dataset is moved between devices
  catalog

  # CPC physically partitioned to 2 operating processor complexes
  Multiprocessor

  # Integrated Coupling Unit
  ICP

  # Logical Partition: subset of processor hardware to supporting an operating system
  LPAR

  # PUs (speciality engine) for coordination of system effort
  ICF catalog

  # Integrated Firmware Processor
  IFP

  # speciality engine: System Assist Processor: PUs to assist PU with workload on I/O; I/O processor for Disaster Recovery:
  SAP

  # speciality engine: dedicated z/OS Application Assist Processors: PUs for execution java GDPScodeGeographically Dispersed Parallel Sysplex: for DR
  zAAPs

  #
  TCB mode

  #
  z/OS XML System Services

  # speciality engine: dedicated z/OS Integrated Information Processors: PUs for accelerated DB2 performance
  zIIPs

  # z Developer Discount
  zDD

  # Application Development Controlled Distribution
  ADCD

  # main storage; inside the CPC cage; up to 384GB
  RAM

  # independent data and control path: between I/O devices and  Memory
  channel

  # Channel To Channel: SCP communicates with another SCP
  CTC

  # formed by mutliple Channel To Channel connections; forms basic sysplex
  CTC ring

  # connection between two CHPIDs
  CTC connection

  # Channel Path Identifier
  CHPID

  # Channel Subsystem
  CSS

  # channels
  OSA / ESCON / FICON

  # Open Systems Adapter
  OSA adapter

  # Direct Access Storage Device: like a hard drive
  DASD

  # commands RESERVE, RELEASE
  Basic shared DASD

  # High Level Qualifier: 1st part of dataset name
  HLQ

  #
  bytes/tracks/cylinders

  # variable (block ?) / fixed (block ?) lenght
  VB / FB

  # Millions of Instructions per Sec: Misleading Indicator of Performance
  MIPS

  # Service Unit / Millions of Service Units
  SU / MSU

  # amount of service units of work (Monitoring)
  SU_SEC

  # Resource Management Facility
  RMF

  # Ctrl-R terminal reset
  XMIT-Lock Mode

  # Attention Identifier
  AID key

  # IBM System z Personal Development Tool, for ISV (Independet Software Vendors)
  zPDT

  # Virtual mainframe - on github
  Hercules.emulator

  #
  https://github.com/mainframed

  # Capacity Backup
  CBU

  #
  ESCON / FICON channels

  # Unit Control Block: for disk devices; small piece of virtual storage
  UCB

  # Dialog Tag Language: source code for ISPF
  DTL

  # Enterprise Storage System
  ESS

  # Parallel Access Volume
  PAV

  # Central STORage: main physical storage - can be shared among LPAR; synchronous access - processor has to wait
  CSTOR

  # access through I/O requests; processor does not wait
  Auxiliary storage

  # Expanded STORage
  ESTOR

  # Hardware Management Console: monitor and control HW (microprocessors)
  HMC

  # Support Element
  SE

  # Real Storage Manager
  RSM

  # Address Space ID - like a process ID in UNIX; z/OS address space is like a UNIX process
  ASID

  # Active Data Dictionary
  ADD

  # Software Configuration and Library Management
  SCLM

  # keyword parameter: Account
  ACCT

  # keyword parameter: Procedure
  PROC

  # Attention
  ATTN

  #  Data Facility Hierarchical Storage Manager - Datenverwaltung and Datensicherung
  DFHSM

  # Migration Control Dataset
  MCDS

  # Backup Control Dataset
  BDCS

  # Virtual Machine: contains minidiscs (Platten)
  VM

  # Data Base / Data Communication Systems
  DB / DC

  # for mathematical problems
  APL

  # Command List: procedural programming language
  CLIST

  # Interactive System Productivity Facility: (GUI) interface for 3270 TSO Terminals; file browser, editor, made of 'panels'
  ISPF

  # Interactive Storage Management Facility
  ISMF

  # ISPF support: Interactive Data Transmission Facility
  IDFT

  # curses-based IBM host access tool / IBM host access tool
  c3270 / x3270

  # ftp: transfer ADN0035.OUT: Host -> PC
  IND$FILE GET 'ADN0035.OUT' ASCII CRLF

  # ftp: Job-Output
  ftp> quote site filetype=jes
  ftp> quote site filetype=seq
  200 SITE command was accepted
  ftp> dir

  # predefined display image that fills the screen
  data entry / menu / list / edit panel

  # ispf: display list of datasets
  DSLIST

  # ispf: display dataset: Command List
  DSLIST CLIST

  # Hierarchical File Manager
  HFS

  # launched from the ISPF-GUI
  start sdsf

  # Interactive System Productivity Facility / Program Development Facility
  ISPF / PDF

  # Program Function (keys): F1 to F24 keys
  PF

  # Program Function Keys: split screen / (repeat)find / (BEF)up / (EOF)down / swap screen / left / right / history
  F2 / F5 / (m)F7 / (m)F8 / F9 / F10 / F11 / F12

  # Program Access (? not Attention ?) Keys
  PA1 / PA2 / PA3

  # Program Access Key for ATTENTION; Left Alt-1
  PA1

  # Program Access Key for RESHOW
  PA2

  # Structured Programming Facility
  SPF

  # Document Composition Facility
  DCF

  # Time Sharing Option (CLI Interface, multi user) - used to run ISPF
  TSO

  # Work Station
  WS

  # Coupling Facility: enables multisystem data sharing in paralles sysplex; contains 1 or more mainframe processor anns special operating system
  CF

  # Coupling Facility: a special LPAR; provides high speed caching, list processing and locking functions in a sysplex; (triangle)
  CF

  # Intelligent Resource Director: Stage 2 of Parallel Sysplex
  IRD

  # z/OS Workload Manager: goal achievement, throughput, responsivenes; ? task scheduler ?
  WLM

  # Dynamic Channel-path Management: for IRD
  DCM

  # Channel Subsystem IO Priority Queueing
  CS IOPQ

  # Supervisor Call
  SVC

  # Geschäftsvorfall Neuanlage
  GV1 / GV 1

  # Geschäftsvorfall Änderung
  GV2 / GV 2

  # Geschäftsvorfall Auskunft
  GV3 / GV 3

  # User Help Desk
  UHD

  # should be like ~ (i.e. $HOME) on unix
  ß

  # TSO Terminal Monitor Program
  TSO TMP

  # von Manfred; VM Vorabauslieferung von Sourcen
  tso vmv s

  # z/OS Unix directory list
  tso udlist

  #
  tso tutor

  #
  tso tabdok

  # mass compile / CICS compile
  tso msvc52t / msvc52a

  #
  tso help something

  #
  tso cancel something

  # list jobs on output queue
  tso status

  # list catalogs
  tso listcat

  # List Dataset ADN0035
  tso listds 'ADN0035.*' / tso dl 'ADN0035.*'

  # network info
  tso netstat home

  # gives /bin/sh; USS
  tso omvs

  # MSV: start MBS Source Verwaltung - based on TSO, ISPF und DB2
  tso msv

  # change status of RACF dbase: switch, (de)activate (without IPL operation)
  tso rvary

  #
  tso rvary list

  # sdsf: start and jump to Status of Jobs (ST) panel
  tso sdsf / (start) sdsf / (start) sdsf;st

  # sdsf: ds: Active users
  SIO - total system start I/O rate

  # tso: execute dataset: ADN0035.REXX(TEST1)
  tso exec 'ADN0035.REXX(TEST1)'

  # tso: submit jcl job: ADN0035.RACFBK.CNTL(HELLOW)
  tso submit 'ADN0035.RACFBK.CNTL(HELLOW)'

  # jcl: data access: Data Control Block
  DCB

  # jcl: Contition Codes: 0: Normal, 4: Warn, 8: Error, 12: Severe Error, 16: Terminal Error
  COND

  # ?
  LISTDSI

  # ispf: exit
  F3 / x / logoff

  # Partitioned Dataset: some kind of folder; contains other datasets (members); sequential datasets; part == members
  PDS

  # tso: PDS: display PDS assigned to DDname SYSPROC, SYSEXEX, ISP*...
  tso isrddn

  # ispf: members of ispf library or tso partitioned dataset; displayed for: edit, browse, view, foreground, batch, and several utilities
  Member Selection List

  # Packed Dataset

  # Partitioned Dataset Extended
  PDSE

  # Restructured Extended Executor Language: python "equivalent" on mainframe
  REXX

  # System Application Architecture
  SAA

  # Common User Access
  CUA

  # file system: Virtual Storage Access Method (clusters)
  VSAM dataset

  # access methods: Virtual Storage Access Method
  VSAM

  # access methods: Queued Sequential Access Method
  QSAM

  # access methods:
  BSAM / ISAM

  # file system: Indexed Storage Access Method (clusters)
  ISAM dataset

  # file system: Direct Access
  DS dataset

  # Common Business Oriented Language: PC based (from MF - Microfocus, MS) / Real Mainframe (VS - Visualize)
  COBOL

  # COBOL program divisions - subdivided to section, sections to paragraphs, paragraphs to sentencies, sentencies to stmts / cmds / instructions / verbs
  Identification / Environment / Data / Procedure

  # permanent / temp - only during program execution
  COBOL Data

  # num - 9 / alphabets - A / alphanumeric - X / sign + or - - S / decimal - V or P
  COBOL data types

  # COBOL
  picture clause

  # COBOL: organize datanames in memory
  level numbers

  # 1 track = 56 664 Bytes
  1 TRK

  # 1 cylinder = 15 TRKs = 849 960 Bytes / Cylinder
  1 CYL

  # IBM utility to create dataset
  IEFBR14

  # creation in: utility / JCL mode
  Dataset

  # kind of a file (when not partitioned) ; Block, Track, Cylinder
  Dataset

  # CNTL / CLIST / ASM / PLI / COBOL / OBJ / LOAD / LIST / OUTLIST / LINKLIST / SCRIPT / DATA
  Dataset types

  # like one line; there is no CR+LF concept; fixed / variable length
  Dataset Record

  # online system: middleware product; Customer Information Control System (command level language); only executes online programs
  CICS

  # CICS system definition file
  CSD

  # online system: middleware product
  IMS

  # middleware product
  DB2

  # Disc Label
  DLBL

  # Link Edit Procedure
  LKED

  # Dataset Commander
  DSC

  # Dataset Control Block
  DSCB

  # Multiple Virtual Storage
  MVS

  # Multiple Virtual Storage/eXtended Architecture
  MVS/EX

  # Multiple Virtual Storage/Enterprise System Architecture
  MVS/ESA

  # Multiple Group Factor Analysis
  MGFA

  # Remote Spooling Communication Subsystem
  RSCS

  # Virtual Machine: components: CP (Control Program) + CMS (Conversational Monitor System); can be used as a general terminal interface for z/VSE app development and system management
  z/VM

  # Virtual Storage Extended: smaller, less complex base for batch and transaction processing; probably contains z/VM
  z/VSE

  # Virtual Storage Extended/Advanced Functions
  z/VSE/AF

  # Transaction Processing Facility: for high speend and high transaction volume; Airlines, Credit Cards
  z/TPF

  #
  zEnterprise

  # zEnterprise Unified Resource Manager
  zManager

  # zEnterprise BladeCenter Extention
  zBX

  # collection of z/Enterprise nodes
  ensemble

  # Customer Initiated Upgrade
  CIU

  # Capacity Upgrade on Demand
  CUD

  # CICS Terminal Owning Region
  TOR

  # Terminal Productivity Executive: multiple session manager in z/OS
  TPX

  # cics: invoke all the master terminal functions (dynamic user control for CICS)
  CEMT

  # compile to LOADLIB
  CEMT S PROG(SDxxx01) new

  #  compile online programs
  CEMT S PROG(OBJX OBJY) PHA

  # Inquiry transaction
  CEMT I TRAN(MBS) new

  # cics: sign on to CICS using a password as authorization from non-3270 terminals with the CESN transaction
  CESN

  # cics: define resources (programs, transactions, files etc.) for the CICS region (system) white the CICS is running
  CEDA

  # cics: Supervisory Terminal
  CEST

  # cics: Command Interpreter
  CECI

  # cics: Transaction to Sign Off from CICS
  CESF
  CESF logoff # cics: Transaction to Sign Off from CICS

  # Program List Tables
  PLT

  # Program List Table Post Initialisation (? initial Process ?) - starting CISC Configuration Manager / Shutdown
  PLTPI / PLTSD

  # System Initialisation Table
  SIT

  # Datenzugriffsschnittstelle
  DZS / MSXZDBS

  # Systemmanager: Transaktion: Verwaltung von Steuerinfo und Steuerung MBS
  MSXZ

  # Address Space Control Block: info and pointers for Address Space Control
  ASCB

  # System / Resource / Job / Task - related
  Control Blocks

  # Task Control Block: unit of work - task; like UNIX threads
  TCB

  # Service Request Block: request for system service - input for SCHEDULE macro
  SRB

  # Recovery Termination Manager
  RTF

  # Authorized Program Facility: for z/OS cross-memory (XM) services
  APF

  # IBM architecture for mainframe computers and peripherals
  zArchitecture

  # System Control Program
  SCP

  # Initial Program Loading operation
  IPL

  # Automatic System Initialisation
  ASI

  # Job Control Language: compile batch and online programs; execute batch programs
  JCL

  # Job Control Statement
  JCS

  # Job Control Command
  JCC

  # Job Control Procedure
  JCP

  # Simultaneous Peripheral Operations Online: Queue
  SPOOL

  # Spooler (System) Display and Search Facility: look at batch output logpool
  SDSF

  # SDSF: output (Browse) / datasets / output descriptors / block delete
  s,v / ? / q /  //p ... //

  # SDSF: Menu entry: Display Active (Users)
  DA

  # sdsf: show sort popup
  sort ?

  # sdsf: edit (and possibly resubmit) the JCL for a given job
  sj

  # sdsf: jump to colname
  loc colname

  # sdsf: filter
  filter ? / filter on/off

  # sdsf: filter on / off
  prefix ? / prefix ADN0035* / prefix (prefix *)

  # sdsf: filter on / off
  owner ? / owner ADN0035* /  owner (owner *)

  # sdsf: display line
  set display on / off

  # sdsf: display possible (Non Protected (NP)) commands
  set action long / short / on / off

  # SDSF: NP commands: list job datasets
  ?

  # SDSF: cmd input: display alternative columns as defined in ISFPARMS
  ?

  # sdsf: Multi Access Queue: display and control members in JES2 MAS
  MAS

  # sdsf: Non Protected / iNPut column
  NP

  # Priority Output Writers Execution Processor and Input Reader
  POWER

  # Data Language One
  DL/I

  # Teleprocessing Monitor
  TP

  # Interactive Interface
  II

  # Interactive Computing
  IC

  # Interactive Computing and Control Facility
  ICCF

  # Resource Access Control Facility: security system; access control and auditing, can be replaced by ACF2, TOPSecret; dbase referencing all the files on filesystem, with access rights
  RACF

  #
  PCF

  # RACF cmd: Definition des Schutzes
  ADDSD

  # RACF cmd: Aendern (Alter) des Schutzes
  ALTSD

  # RACF cmd: Delete des Schutzes
  DELSD

  # ? RACF cmd: ?
  LISTDS

  # Universal Access
  UACC

  # Query Management Facility: send SQL queries to IBM DB2
  QMF

  #
  ITR Ratio

  # Large System Performance Reference
  LSPR ratio

  # I/O Control Dataset: translate physical I/O addresses into device numbers used by OS to access a device
  IOCDS

  # Hardware Save Area: special storage for device numbers
  HSA

  # Return Code
  $RC

  # Shared Virtual Area
  SVA

  # System Directory List
  SDL

  # Conversational Monitoring System: REXX EXECs (programs) are running in CMS mode
  CMS

  # Channel Unit Address
  CUA

  # Access Control Block
  ACB

  # Block Mark / Tape Mark
  BM / TM

  # Entry Sequenced - / Key Sequenced - / Relative Record - Dataset
  ESDS / KSDS / RRDS

  # Return Code
  $RC

  # Max Return Code
  $MRC

  # Abnormal End Code
  $ABEND

  # External Symbol Dictionary
  ESD

  # Relocation Dictionary
  RLD

  # End Of Object Module
  END

  # End of Procedure
  EOP

  # Control Interval
  CI

  # Unix System Services
  USS

  # Virtual Telecommunications Access Method
  VTAM

  # Volume Table of Content: structure with metadata for Basic shared DASD
  VTOC

  # Volume Serial: Disk volume (pack) identification
  VOLSER

  # rexx:
  DATE('E')

  # rexx: hexadecimal
  say 'C1 81'x

  # rexx: read user input
  pull varname

  # rexx: line continuation
  ;

  #
  if ... then do ... end else do... end

  # Invokes z/OS Unix Shell, i.e. gives /bin/sh; root is in BPX.SUPERUSER
  OMVS

  # Job Entry Subsystem: JES2 / JES3
  JES

  # file transfer
  IND$FILE

  # Transaction Processing Facility
  TPF

  # Boundary
  BNDS

  # Master Console: kind of root; 'SPECIAL'

  # Processor Resource/Systems Manager - part of LPAR hypervisor
  PR/SM (PRSM)

  # SW layer; manages mutliple OSes running in single CPC; mainframe uses type 1 (native) hypervisor
  hypervisor

  # native: software running directly on HW platform
  type 1 (native) hypervisor

  # hosted: software running withing an operating system environment (e.g. VMWare)
  type 1 (hosted) hypervisor

  # Transmit
  XMIT / Recieve

  # Record Format: optained by by x = Listdsi(your-dataset-name)
  SYSRECFM

  # Logical Record Length
  SYSLRECL

  # Record Length: optained by by x = Listdsi(your-dataset-name)
  RECL

  # Logical Record Length: optained by by x = Listdsi(your-dataset-name)
  LRECL

  # Allocation in space units
  SYSALLOC

  # Dataset Organisation: PS / PSU / DA / DAU / IS / ISU / PO / POU / VS / ???
  SYSDSORG

  # Read / write data to / from dataset (Not a part of REXX standard)
  EXECIO

  # Allocate / Free dataset
  ALLOC / FREE

  # Disposition
  DISP

  # Record Format: initial view for unformated / blocked (FB, VB, ...) dataset formats
  RECFM=U / RECFM=BLK

  # Record Format: fixed (blocked) / variable (blocked)
  RECFM=F (=FB) / RECF=V (=VB)

  # Record Format: fixed blocked; several logical records (lines) in one physical block
  RECFM=FB

  # Record Format: Variable; one logical record in one physical block: RDW<Data>
  RECFM=V

  # Record Format:
  RECFM=VB

  # Record / Block Descriptor Word: for RECFM=VB
  RDW / BDW

  # Record Format: fixed blocked record; control chars: ANSI / Machine code
  RECFM=FBA / RECFM=FBM

  # Rexx instruction (routine) / control of traps
  CALL

  # DB2 utility: control statement; loading vals into a table
  LOAD

  # TSO/E cm; specify private load libs: add / remove / acti- / deactivate / display
  STEPLIB

  # Simmetrical MultiProcessors; ? RISC systems ?
  SMP

  # Global Resource Serialization function
  GRS

  # A systems complex: z/OS images in one unit; uses messaging services
  Sysplex

  # Multiple mainframes acting as one; sysplex that uses one or more Coupling Facilities
  Parallel Sysplex

  # Sysplex Failure Manager - policy
  SFM

  # Automatic Restart Manager: system recovery function; improves availability of batch jobs and started tasks
  ARM

  # Disaster Recovery:
  DR

  # Geographically Dispersed Parallel Sysplex: for DR
  GDPS

  # Server Time Protocol: implemented in LIC (Licensed Internal Code)
  STP

  # on servers
  TOD clock

  # Dynamic Address Translation
  DAT

  # Region / Segment / Page / Block Index - parts of a Virtual Address
  RX / SX / PX / BX

  # System Resource Manager
  SRM

  # Program Status Word
  PSW

  # Residence Mode: a program attribute
  RMODE

  # Addressing Mode
  AMODE

  # System Queue Area
  SQA

  # Common Storage Area
  CSA

  # Parameter Library: secondary subsystem
  PARMLIB

  # Cursor / Page
  scroll CSR / PAGE

  # Dataset (Storage) Organisation: Physical Sequential / Partitioned Organized / Direct
  DSORG=PS / DSORG=PO / DSORG=DA

  #
  HOSTVARS / PROCVARS

  #
  DSNUPROC

  # DB2 subsystem library
  prefix.SSPGM

  # DB2 subsystem / ? Dataset Name ?
  DSN

  # Supplied JCL procedure; Invoke DB2 online utitity
  DSNUPROC

  # Data Definition: describe dataset, specify I/O resources for DD
  DD-Record

  # patch applied by a system programmer
  PTF

  # Fix Centrall
  FC

  # text commands: Text Enter / Text Split / Text Flow
  TE / TS / TF

  # conversions: Lower Case / Upper Case
  LC / UC

  # Data Definiton
  DDNAME

  # Dataset Name
  DSNAME

  # some kind of network
  SNA

  # Dialog Test
  DTEST

  # ispf: Trace TPUT, TGET, PUTLINE buffers; produce ABEND dumps; gather terminal status info
  ENVIRON

  # ispf: display current LIBDEF info
  ISPLIBD

  # ispf: toggle forms of the F keys
  FKA

  # ispf: print logical screen in uniform chars
  PRINTL

  # recall previous commands to the command line
  RETRIEVE

  # member selection lists:
  FIND

  # member selection lists: repeat find
  RFIND

  # ispf: Shared Profile
  SHRPROF

  # member selection lists: Search For
  SRCHFOR

  # member selection lists: SELECT
  SEL

  # Vertical Screen Split
  SPLIV

  # Jump to logical screens - see SPLIT
  SWAP

  # ispf: move pop-up displayed on the screen
  WINDOW

  # ispf: initiate workstation connection / disconnect user from workstation
  WSCON / WSDISCON

  # SESM Session Manager Mode; ISPF settings
  ISPFVAR

  # ISPF Dialog Tag Language Compiler
  ISPDTLC

  # member selection lists: Match 1 char for member names
  %

  # member selection lists: Match any number of chars for member names
  ,*

  # member selection lists: select / print / rename / delete / edit / view /browse
  S / P / R / D / E / V / B

  # Common Criteria security evaluation: Evaluation Assurance Levels
  EAL1 - EAL7

  # Online Transaction Processing: interactively with user
  OLTP

  # Transactions Per Second
  TPS

  # application menu - set screen 1
  action bar

  # F1: Kommandos: Compilieren des Objekts aus MSV heraus
  CP

  # uss: edit unixfile.txt from under the host editor
  oedit unixfile.txt

  # Copy a text file to a text file
  cp unixfile.txt "//'stuff.x'"

  # Copy a text file to a text file
  cp "//'stuff.x'" "//'stuff.y'"

  # Copy a text file to a sequential dataset
  cp -T source_file "//'hlq.desti(nation)'"

  # Copy a binary file to a sequential dataset
  cp –B source_file "//'hlq.desti(nation)'"

  # Copy an executable binary (a program object) to a PDS/E
  cp –X source_pgm "//'hlq.desti(nation)'"

  # Reliability, Availability and Serviceability
  RAS

  #+BEGIN_SRC bash :results output
    # SDSF from java
    java -Djava.library.path=/usr/lib/java_runtime64 -jar /usr/include/java_classes/isfjcall.jar
  #+END_SRC
}

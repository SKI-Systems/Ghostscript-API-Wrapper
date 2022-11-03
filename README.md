# Ghostscript-API-Wrapper ![GitHub](https://img.shields.io/github/license/SKI-Systems/Ghostscript-API-Wrapper?label=License&style=plastic)

### The Ghostscript-API-Wrapper is an open source project, that simplify use of Ghostscript for Delphi and Free Pascal.

### Features:
- Integration of often used Ghostscript parameters with description
- A simple PDF converter to create PDF and PDF-A files
- Page preview as an image
- Execution of an operation in a single thread
- Output of debug informations from the wrapper and the Ghostscript library
- Supports 32-Bit and 64-Bit Windows applications
- Example applications for Delphi and Lazarus

### Ghostscript 

At the following links you can find the [website](https://www.ghostscript.com) and [documentation](https://ghostscript.readthedocs.io) of Ghostscript.
![Ghostscript Icon](https://gdm-catalog-fmapi-prod.imgix.net/ProductScreenshot/fdea9f1c-d655-48a3-bb5a-4c51815bb294.png) 


### CHANGELOG

#### version 1.00.2 (FPC and Delphi Win32/64)
- added example for Lazarus/FPC and changed folders of the examples
- improved internal documentation
- fixed an issue on creating the image preview 64-Bit
- fixed an issue with threads in the examples

#### version 1.00.1 (FPC and Delphi Win32/64)
- FPC/Lazarus support
- auto call of gsapi_quit
- include gsapi_run* operation
- added display_memfree to the compiler switch USE_GSDisplayMemAlloc to avoid memory leaks of Ghostscript
- fixed an issue with the internal log

#### first version 1.00.0 (Delphi Win32/64)
- initial commit
  - wrapper units
  - TGS_Api to use the wrapper units
  - TGS_Converter simple PDF/PDF_A converter 
  - SkiSys.GS_ParameterTypes integrated Ghostscript parameters with description based on the Ghostscript doumentation
  - Example Project (Delphi)

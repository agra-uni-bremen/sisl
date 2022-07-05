# SISL User Manual

User manual for the *Scheme-based Input Specification Language* (SISL).
SISL is a *Domain-Specific Language* (DSL) for describing highly
structured binary input formats (e.g. protocol message formats) for
[concolic software testing][ct wikipedia].

## Download

To install SISL, first download the source code and accompanying example
applications by clicking on [this link][sisl download]. By clicking on
the link, you will download a ZIP archive containing the SISL source
code. Information on how to install the SISL tooling, using this ZIP
archive, are provided below.

## Directory Organization

The important subdirectories in the downloaded archive are:

* `examples/`: Contains example RIOT application demonstrating the use of SISL.
* `vp/`: Contains the source code of our SISL enhanced version of SymEx-VP.
* `sisl/`: Includes the source code of the SISL R7RS Scheme library.

Refer to the source code in the subdirectory of each component for implementation details.

## Installation

The SISL tooling can be installed on Linux/Windows/macOS using
[Docker][docker web]. In order to use this tool, Docker therefore needs
to be installed first on your system. If Docker isn't installed yet,
please follow the official Docker installation instructions for your
operating system, see:

* [Install Docker on Ubuntu Linux][docker linux]
* [Install Docker on Mac][docker mac]
* [Install Docker on Windows][docker windows] (WSL 2 Backend is recommended)

After installing Docker, extract the `sisl.zip` file downloaded from
above, open a command-line shell and navigate to the directory
containing the SISL source code on your computer (refer to your
operating system documentation on how to do that). In the SISL source
code directory (which should include a `Dockerfile`), run the following
command to build the SISL Docker image:

	$ docker build -t sisl .

Depending on your hardware, this may take between 5 and 10 minutes.  If
the command was executed successfully, a Docker image for SISL was
created. You can verify that this is the case by checking the output of
the `docker images` command (see the [Docker documentation][docker
images] for more details). The generated `sisl` image can then be
started from your operating system command-line shell using the
following command:

	$ docker run -it sisl

This will spawn a new interactive Linux command-line shell environment
from which the SISL tool can be used. In case you want to quickly run
an example application, invoke the following command *from within the
Docker container*:

	$ make -C /home/sisl/sisl-vp/examples/sisl-network-stack/ simulate

The `sisl-network-stack` example application and the associated SISL
specification are described in greater detail below.

## Usage Example

In order to utilize the SISL tooling, a RISC-V application which processes input in a binary input format is required.
For this purpose, a network application for the [RIOT operating system][riot web] is provided in the `./examples/sisl-network-stack/` directory.
This application receives network packets via the [*Serial Line Internet Protocol*][rfc1055] (SLIP) interface provided by SymEx-VP.
In this scenario, SISL can be employed to model the input received over this SLIP interface by the example RIOT application.

The example RIOT application utilizes a minimal configuration of the RIOT network stack and uses IPv6.
That is, it expects input to conform to the [IPv6 message format][ipv6 message format], consisting of a header and a payload.
The IPv6 header is defined as follows:

<figure style="text-align: center;">
<img src="https://upload.wikimedia.org/wikipedia/commons/4/4c/IPv6_header-en.svg" />

<figcaption>IPv6 header message format (CC BY-SA 4.0 <a href="https://commons.wikimedia.org/wiki/File:IPv6_header-en.svg">Michel Bakni</a>).</figcaption>
</figure>

A SISL specification which targets this input format could look as follows:

	(define-input-format (ipv6-packet next-hdr &encapsulate payload)
	  (make-uint 'verison-field 4 ipv6-version-value)
	  (make-uint 'traffic-class 8 #x0)
	  (make-uint 'flow-label 20 #x0)
	  (make-uint 'payload-length 16 (input-format-bytesize payload))
	  (make-uint 'next-header 8 next-hdr)
	  (make-uint 'hop-limit 8 #x42)
	  (make-symbolic 'src-addr 128)
	  (make-symbolic 'dest-addr 128))

This defines an input format which expects a `next-hdr` parameter and defines the fields of the format.
Some fields are set to a fixed value while others depend on parameters (e.g. the `next-header` or `payload-length` field).
Refer to the [SISL Language Reference](#SISL-Language-Reference) below for details.
The address fields have a complicated internal structure, instead of replicating this structure in the specification they are declared as symbolic.
This causes the concolic testing engine, provided by SymEx-VP, to automatically infer appropriate values for them.

From this SISL specification a low-level specification can be generated using the following code:

	(write-format
	  (ipv6-packet
	    icmpv6-next-header
	    (make-symbolic 'body (bytes->bits 32))))

This creates an `ipv6-packet` instance with a `'next-hdr` value of `icmpv6-next-header` and a payload consisting of a sequence of 32 unconstrained symbolic bytes.
The generated low-level specification can then be passed to SymEx-VP to test the `./examples/sisl-network-stack/` RIOT application.

The full input specification for the aforementioned RIOT application can be found in `./examples/sisl-network-stack/input-format.scm`.
In order to execute the `sisl-network-stack` application with this input format specification, run the following command from the `./examples/sisl-network-stack` subdirectory:

	examples/sisl-network-stack$ make simulate

This will automatically generate the low-level specification and execute the application with it using SymEx-VP.
SymEx-VP will then enumerate reachable paths through the program based on the symbolic values (e.g. the symbolic payload).
For each executed path, a lot of debug information will be output.
As such, the output for a single execution path may look as follows:

```
##
# 192th concolic execution
##

ipv6: waiting for incoming message.
RIOT network stack example application
gnrc_netif_raw: reallocating.
00000000  60  00  00  00  00  20  DE  42  FF  DE  1C  FE  80  1E  FD  00
00000010  5E  5E  00  00  00  00  00  5E  00  00  00  00  00  00  00  00
00000020  00  00  00  00  00  00  00  01  5E  00  00  00  00  00  00  5E
00000030  5E  40  5E  5E  00  00  00  00  00  00  00  00  00  5E  00  00
00000040  00  00  5E  00  00  5E  00  00
ipv6: GNRC_NETAPI_MSG_TYPE_RCV received
ipv6: Received (src = ffde:1cfe:801e:fd00:5e5e::5e, dst = ::1, next header = 222, length = 32)
ipv6: forward nh = 222 to other threads

Info: /OSCI/SystemC: Simulation stopped by user.
```

The RIOT application is build with a lot of debug output enabled to make it easier to comprehend how changes to the SISL input specification affect the concolic testing engine and the paths explored by it.
To experiment with this example application, try changing individual fields of the specification in `./examples/sisl-network-stack/input-format.scm`, run `make simulate` again and see how your changes affect the output.

Refer to `./examples/README.md` for more information on provided example applications.

## SISL Language Reference

SISL allows specifying binary input formats as a sequence of variable-width fields of different types.
The core concepts of the SISL language are therefore: (1) *Input Formats* which consist of one or more variable-width fields and (2) *Input Fields* which correspond to variable-width bit blocks in the targeted input format specification.
Both concepts are further described in the following subsections.

### Input Formats

Input formats can either be defined using the `define-input-format` macro (analog to how Scheme procedures are normally defined) or instantiated directly using the `make-input-format` procedure.
Usually, the former is preferred as it allows defining parametrisable input formats which can be instantiated according to given parameters.
As an example, consider the following input format definition:

	(define-input-format (my-input-format some-parameter)
	  (make-uint 'start-frame 8 #xFE)
	  (make-uint 'value-byte 8 some-parameter)
	  (make-uint 'end-frame 8 #xFF))

The code above defines a new exemplary input format called `my-input-format` which takes one parameter (named `some-parameter`) and consists of three concrete fields: `start-frame`, `value-byte`, and `end-frame`.
All fields are one byte in size, the `start-frame` and `end-frame` field have a fixed hexadecimal value (`0xFE` and `0xFF`), the `value-byte` field depends on the value of the defined parameter.
As an example, the defined input format could be instantiated as follows:

	(my-input-format #x42)

This would result in an input format equal to the following byte sequence: `[0xFE, 0x42, 0xFF]`.
By utilizing `make-input-format` (i.e. without defining a parametrisable input format) the same byte sequence would be generated by the following code:

	(make-input-format
	  (make-uint 'start-frame 8 #xFE)
	  (make-uint 'value-byte 8 #x42)
	  (make-uint 'end-frame 8 #xFF))

The advantage of `define-input-format` is that the same specification can be re-used multiple times, we envision that SISL support libraries are created in the future which provide ready-to-use parametrisable definitions for common binary input formats.
Of course, an input format specification which corresponds to a fixed size of concrete bytes is nonsensical in the concolic testing context.
The description of input format fields below will describe how concrete and symbolic fields can be combined in input formats.
The remainder of this section will illustrate how different input formats can be encapsulated in each other and how a machine-readable low-level specification can be generated from the human-readable SISL specification.

#### Format Encapsulation

Different input formats, defined using `define-input-format` can also be encapsulated in each other.
This is especially useful when modelling network protocols using sisl.
For example, consider the following simplified code:

	(define-input-format (ethernet-frame &encapsulate payload)
	  ...)

	(define-input-format (ipv6-packet &encapsulate payload)
	  ...
	  (make-uint 'payload-length 16 (input-format-bytesize payload))
	  ...)

	(define-input-format (udp-packet &encapsulate payload)
	  ...)

	(ethernet-frame
	  (ipv6-packet
	    (udp-packet
	      (make-symbolic 'payload 128))))

This code defines three input formats: [`ethernet-frame`][ethernet wikipedia], [`ipv6-packet`][ipv6 wikipedia], [`udp-packet`][udp wikipedia].
Furthermore, the code encapsulates this formats according to the [TCP/IP model][tcp/ip wikipedia] used on the Internet.
The individual fields of these input formats have been largely omitted for clarity.
The important thing to take away from this code is that each of these format definitions uses the `&encapsulate` keyword to denote that an additional input format (named `payload`) is appended to the end of the defined one.
The defined fields can also operate on this encapsulated `payload` (e.g. the size of the `payload-length` field in the `ipv6-packet` definition is set to the byte size of the encapsulated `payload`).

The encapsulated must be defined when instantiating the defined formats.
According to the instantiation at the very end of the code above, a symbolic payload is encapsulated in a UDP packet which is encapsulated in an IPv6 packet, which is ultimately encapsulated in an Ethernet frame.
The `make-symbolic` procedure is further discussed in the description of input fields below.

#### The Low-Level Specification

Since SISL input format specifications are executed R7RS Scheme programs, it is difficult to parse them correctly.
For this reason, SISL provides a procedure called `write-format` which generates an easy machine-readable format of the provided input format specification.
This procedure expects an instantiated input format and can, for example, be invoked as follows:

	(write-format
	 (ethernet-frame
	   (ipv6-packet
	    (udp-packet
	     (make-symbolic 'payload 128)))))

This will write the defined input format in a machine-readable [Bencode encoding][bencode wikipedia] to standard output.
This output can be redirected to a file and passed to our provided version of SymEx-VP to perform concolic testing of RISC-V software based on the provided format.

### Input Fields

Input fields are the other central concept of the SISL language.
As we have seen above, each input format consists of a sequence of input fields.
In the examples above, we have primarily created input fields using the `make-uint` procedure.
However, there are actually two types of different input fields which can be created using different procedures:

1. *Concrete Fields:* These fields have a concrete value (e.g. `4`, `3248902`, or `-1`) and can be created using the procedures `make-uint`, `make-sint` or `make-concrete`.
2. *Symbolic Fields:* These fields a symbolic value which can (optionally) be constrained to only represent a certain range of values (e.g. `X > 5 && X < 10`). These fields can only be created using `make-symbolic`.

Both field types are described in greater detail below.
Independent of the field type, each field has a name (represent by a Scheme symbol), a fixed size and a value (which differs depending on the field type).

#### Concrete Fields

Concrete fields are fields with a concrete value.
The most low-level procedure to create concrete fields is `make-concrete`.
This procedure expects a field name, a field size, and a field values as a R7RS bytevector.
For example, the procedure may be invoked as follows:

	(make-concrete 'c1 16 #u8(23 42))

This creates a two byte field with the name `c1` with a bytevector of the value `[23, 42]`.
If the size of the bytevector is smaller than the given field size, then the bytevector value is padded with zeros.

Based on the `make-concrete` procedure two utility procedures are provided which ease the creation of fields with concrete integer values: `make-sint` and `make-uint`.
Both procedures a similar to `make-concrete` in the sense that they require the same parameter, except that the value parameter is an integer and not a bytevector.
The `make-sint` procedure creates fields with a [two's complement][twocomp wikipedia] signed integer value.
The `make-uint` procedure creates fields with an unsigned integer value.
As such, an example signed integer field may be created as follows:

	(make-sint 'negative 8 -128)

While an unsigned integer field may be created using:

	(make-uint 'positive 8 255)

If the given integer value cannot be represented in the given amount of bits, an error is raised.
All concrete fields default to network byte order (e.g. big endian).
The byte order can be explicitly changed using the `field-be` and `field-le` procedures`.
For example:

	(make-input-format
	  (field-le (make-concrete 'c1 16 #(#x23 #x42)))
	  (field-le (make-uint 'c5 32 #x2342)))

#### Symbolic Fields

Symbolic fields are similar to concrete fields except that they use symbolic instead of concolic field values.
Symbolic input fields are created using the `make-symbolic` procedure.
In order to create an unconstrained symbolic field, the procedure would be invoked as follows:

	(make-symbolic 'unconstrained 64)

This would create an unconstrained symbolic value of 64 bits.
However, it is often desirable to specify constrains for symbolic values (e.g. to confine the value to a certain range).
This can be achieved by passing [KLEE KQuery][kquery] expressions as an option third argument.
As Kquery is also based on S-Expressions, these constraints are expressed as Scheme quasiquotations within SISL.
For example:

	(define icmpv6-na 134)
	(define icmpv6-ns 135)

	(make-symbolic 'type 8 `((Or
	                            (Eq type ,icmpv6-na)
	                            (Eq type ,icmpv6-ns))))

The code above would constrain the symbolic `type` variable in a way that either has the value of the constant `icmpv6-na` or that of the constant `icmpv6-ns`.
The first parameter of the `Eq` operation in the constrained refers to the name of the symbolic field.
As such, it is also possible to define constraints based on prior fields, e.g.

	(define-input-format my-format
	  (make-symbolic 'first 8)
	  (make-symbolic 'second `((Not (Eq first second)))))

In this example, the field with the name `second` is constrained to never have the same value as the field with the name `first`.
Also note that the third parameter to `make-symbolic` is a list of constrains, hence it is possible to pass multiple constraints.
For example:

	(make-symbolic 'field `((Ult first 10) (Ugt first 5)))

Which would constraint `field` to `field < 10 && field > 5`.
Please refer to the [KQuery documentation][kquery] for more information on expressing constraints for symbolic input fields in SISL.

### Miscellaneous

Apart from procedures to create input formats and input format fields, SISL also provides a few utility procedures.
These are listed below:

* `input-format-bitsize`: Returns size of a given input format in bits.
* `input-format-bytesize`: Returns size of a given input format in bytes.
* `u{8,16,32,64}`: Syntactic sugar to create unsigned 8, 16, 32, and 64 bit integers.
* `s{8,16,32,64}`: Syntactic sugar to create signed 8, 16, 32, and 64 bit integers.
* `bits->bytes`: Convert the given amount of bits to bytes.
* `bytes->bits`: Convert the given amount of bytes to bits.

Additionally, keep in mind that each SISL specification is a valid Scheme program.
For this reason, it is possible to use Scheme language features (e.g. procedures, variables, constants, …) in a SISL specification.
For example, as we have seen above, Scheme constants can be used to avoid magic numbers in defined input formats (e.g. `(define icmpv6-na 134)`).

## Acknowledgements

This work was supported in part by the German Federal Ministry of
Education and Research (BMBF) within the project Scale4Edge under
contract no. 16ME0127 and within the project VerSys under contract
no. 01IW19001.

[docker web]: https://www.docker.io/
[docker linux]: https://docs.docker.com/engine/install/ubuntu/
[docker windows]: https://docs.docker.com/desktop/windows/install/
[docker mac]: https://docs.docker.com/desktop/mac/install/
[docker images]: https://docs.docker.com/engine/reference/commandline/images/
[riot web]: https://riot-os.org
[uart wikipedia]: https://en.wikipedia.org/wiki/Universal_asynchronous_receiver-transmitter
[ct wikipedia]: https://en.wikipedia.org/wiki/Concolic_testing
[ethernet wikipedia]: https://en.wikipedia.org/wiki/Ethernet_frame
[ipv6 wikipedia]: https://en.wikipedia.org/wiki/IPv6_packet
[udp wikipedia]: https://en.wikipedia.org/wiki/User_Datagram_Protocol#UDP_datagram_structure
[tcp/ip wikipedia]: https://en.wikipedia.org/wiki/TCP/IP_model
[bencode wikipedia]: https://en.wikipedia.org/wiki/Bencode
[twocomp wikipedia]: https://en.wikipedia.org/wiki/Two%27s_complement
[r7rs scheme]: https://small.r7rs.org/
[ipv6 message format]: https://datatracker.ietf.org/doc/html/rfc8200#section-3
[rfc4443]: https://datatracker.ietf.org/doc/html/rfc4443
[rfc1055]: https://datatracker.ietf.org/doc/html/rfc1055
[kquery]: https://klee.github.io/docs/kquery/
[sisl download]: https://user.informatik.uni-bremen.de/~tempel/sisl/sisl.zip
[sisl artifacts]: https://user.informatik.uni-bremen.de/~tempel/sisl/artifacts.zip

\name{CoCoCore-internal}
\title{Internal CoCoCore objects}
\alias{.after.set.current}
\alias{.before.set.both}
\alias{.before.set.current}
\alias{.coco.command.implemented}
\alias{.coco.id}
\alias{.coco.load}
\alias{.empty.string}
\alias{.encode}
\alias{.encode.model}
\alias{.encode.model.1}
\alias{.encode.type.and.options}
\alias{.encode.visit}
\alias{.false.if.NULL}
\alias{.First.lib}
\alias{.First.lib.CoCoCore}
\alias{.get.cocolib.name}
\alias{.get.lib.and.tmp}
\alias{.get.rs.cshlib.name}
\alias{.is.current.model}
\alias{.is.nil.model}
\alias{.Last.lib}
\alias{.mips.load}
\alias{.my.ascii}
\alias{.my.dyn.load.open}
\alias{.my.getenv}
\alias{.my.test.file.found}
\alias{.my.trace}
\alias{.object.of.models}
\alias{.onAttach}
\alias{.onLoad}
\alias{.packageName}
\alias{.return.factor.type.list}
\alias{.return.level.list}
\alias{.return.missing.list}
\alias{.return.name.list.string}
\alias{.set.acceptance}
\alias{.set.components}
\alias{.set.current.model}
\alias{.set.rejection}
\alias{.set.separators}
\alias{.split.data}
\alias{.table.value}
\alias{.to.search}
\alias{.visit.interval}
\alias{.visit.model}
\alias{.visit.models}

%   \alias{CoCoIdClass-class}
%   \alias{CoCoClass-class}
%   \alias{CoCoModelClass-class}
%   % \alias{CoCoTestClass-class}
%   
%   \alias{label} % CoCoTestClass-method
%   % \alias{testLabel} % CoCoTestClass-method
%   \alias{width} % CoCoTestClass-method
%   % \alias{testWidth} % CoCoTestClass-method
%   
%   \alias{dynamic.Graph} % CoCoModelClass-method
%   % \alias{dynamic.Graph.Old} % CoCoModelClass-method
%   \alias{modifyModel} % CoCoModelClass-method
%   \alias{subModifyModel} % CoCoModelClass-method
%   % \alias{subModifyModel.Old} % CoCoModelClass-method
%   \alias{testEdge} % CoCoModelClass-method
%   % \alias{testEdge.Old} % CoCoModelClass-method
%   
%   \alias{makeModel} % CoCoObjects
%   \alias{newCoCoTestObject} % CoCoObjects
%   \alias{recover.coco.model} % CoCoObjects

% call.coco.R		  :
% 
% call.coco.message.R	  : All arguments, return ok.coco(result)
% 
% call.coco.simple.R	  : Only code and subcode as argument, return subcode or NULL
% coco.simple.command.R	  : ~ call.coco.simple.R
% 
% call.coco.chars.R	  : Return string
% coco.enter.string.R	  : = call.coco.chars.R
% call.coco.longs.R	  : Return longs
% call.coco.reals.R	  : Return reals
% 
% coco.enter.all.R	  : ~ coco.replace.all.R
% coco.replace.all.R	  : ~ coco.enter.all.R
% 
% coco.simple.model.R	  : -> coco.simple.command.R
% coco.simple.double.R	  : -> coco.simple.command.Rb
% coco.string.double.R	  : -> coco.enter.string.R
% coco.string.model.R	  : -> coco.enter.string.R
% 
% coco.init.R		  :
% coco.load.R		  :
% coco.resume.R		  :
% coco.start.R		  :

\description{Internal CoCoCore objects.
        This package contains the shared library of CoCo, with the 
        interface function "call.coco" for calling CoCo from R and Splus,
        some extensions and specializations to this function
        and some auxiliary functions of the interface to CoCo.
}
\details{The functions of this package are not to be called
         directly by the user. Please use the functions of the
	 package "CoCoRaw" (and "CoCoObjects" and "CoCoGraph"). 
         Datasets are found in the package "CoCo" and "CoCoCg". }

\keyword{internal}

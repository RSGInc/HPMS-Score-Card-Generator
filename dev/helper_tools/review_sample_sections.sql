USE [HPMS]
GO

/****** Object:  Table [dbo].[Review_Sample_Sections]    Script Date: 11/16/2021 2:39:36 PM ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO

CREATE TABLE [dbo].[Review_Sample_Sections](
	[DataYear] [int] NULL,
	[StateId] [int] NULL,
	[RouteId] [varchar](255) NULL,
	[BeginPoint] [float] NULL,
	[EndPoint] [float] NULL,
	[SectionLength] [float] NULL,
	[SampleId] [varchar](255) NULL,
	[ExpansionFactor] [float] NULL,
	[Comments] [varchar](255) NULL,
	[LastModifiedOn] [varchar](255) NULL,
	[LastModifiedBy] [varchar](255) NULL,
	[Invalid] [int] NULL,
	[StateYearKey] [int] NULL
) ON [PRIMARY]
GO


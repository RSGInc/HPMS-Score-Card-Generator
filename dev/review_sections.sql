USE [HPMS]
GO

/****** Object:  Table [dbo].[Review_Sections]    Script Date: 11/16/2021 2:39:56 PM ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO

CREATE TABLE [dbo].[Review_Sections](
	[DataYear] [int] NULL,
	[StateId] [int] NULL,
	[RouteId] [varchar](255) NULL,
	[BeginPoint] [float] NULL,
	[EndPoint] [float] NULL,
	[DataItem] [varchar](255) NULL,
	[SectionLength] [float] NULL,
	[ValueNumeric] [float] NULL,
	[ValueText] [varchar](255) NULL,
	[BeginDate] [datetime] NULL,
	[StateYearKey] [int] NULL
) ON [PRIMARY]
GO


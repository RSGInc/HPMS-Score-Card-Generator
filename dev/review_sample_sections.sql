USE [HPMS]
GO

/****** Object:  Table [dbo].[Review_Sample_Sections]    Script Date: 11/16/2021 2:39:36 PM ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO

CREATE TABLE [dbo].[Review_Sample_Sections](
	[Year_Record] [int] NULL,
	[State_Code] [int] NULL,
	[Route_ID] [varchar](255) NULL,
	[Begin_Point] [float] NULL,
	[End_Point] [float] NULL,
	[Section_Length] [float] NULL,
	[Sample_ID] [varchar](255) NULL,
	[Expansion_Factor] [float] NULL,
	[Comments] [varchar](255) NULL,
	[Last_Modified_On] [varchar](255) NULL,
	[Last_Modified_By] [varchar](255) NULL,
	[Invalid] [int] NULL,
	[StateYearKey] [int] NULL
) ON [PRIMARY]
GO


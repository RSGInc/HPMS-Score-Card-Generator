USE [HPMS]
GO

/****** Object:  Table [dbo].[Review_Sections]    Script Date: 11/16/2021 2:39:56 PM ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO

CREATE TABLE [dbo].[Review_Sections](
	[Year_Record] [int] NULL,
	[State_Code] [int] NULL,
	[Route_ID] [varchar](255) NULL,
	[Begin_Point] [float] NULL,
	[End_Point] [float] NULL,
	[Data_Item] [varchar](255) NULL,
	[Section_Length] [float] NULL,
	[Value_Numeric] [float] NULL,
	[Value_Text] [varchar](255) NULL,
	[Value_Date] [datetime] NULL,
	[StateYearKey] [int] NULL
) ON [PRIMARY]
GO


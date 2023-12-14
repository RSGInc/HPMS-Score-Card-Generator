USE [HPMS]
GO

/****** Object:  Table [dbo].[Timelinesstable]    Script Date: 11/16/2021 2:40:16 PM ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO

CREATE TABLE [dbo].[Timelinesstable](
	[DataYear] [int] NULL,
	[StateId] [int] NULL,
	[Submitted_By] [varchar](255) NULL,
	[Submitted_On] [datetime] NULL,
	[SubmissionNumber] [int] NULL
) ON [PRIMARY]
GO


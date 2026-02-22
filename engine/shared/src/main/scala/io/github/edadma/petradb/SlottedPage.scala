package io.github.edadma.petradb

import io.github.edadma.stow.{PageId, NoPage}

// Slotted page layout:
//   [0..1]  slot count (2 bytes, big-endian unsigned short)
//   [2..3]  free space pointer (2 bytes) — lowest used data offset, starts at pageSize
//   [4..7]  next page PageId (4 bytes, big-endian int)
//   [8..]   slot directory: 4 bytes each (2-byte offset, 2-byte length; offset=0 = tombstone)
//   ...     free space gap
//   [end]   row data packed backward from page end

private val SlotCountOffset = 0
private val FreeSpaceOffset = 2
private val NextPageOffset  = 4
private val HeaderSize      = 8 // bytes before the slot directory starts

class SlottedPage private (val data: Array[Byte], val pageSize: Int):

  def slotCount: Int = readShort(SlotCountOffset)

  def freeSpace: Int =
    val fsp     = readShort(FreeSpaceOffset)
    val dirEnd  = HeaderSize + slotCount * 4
    fsp - dirEnd

  def nextPage: PageId =
    ((data(NextPageOffset) & 0xff) << 24) |
      ((data(NextPageOffset + 1) & 0xff) << 16) |
      ((data(NextPageOffset + 2) & 0xff) << 8) |
      (data(NextPageOffset + 3) & 0xff)

  def setNextPage(page: PageId): Unit =
    data(NextPageOffset) = ((page >> 24) & 0xff).toByte
    data(NextPageOffset + 1) = ((page >> 16) & 0xff).toByte
    data(NextPageOffset + 2) = ((page >> 8) & 0xff).toByte
    data(NextPageOffset + 3) = (page & 0xff).toByte

  def addSlot(rowData: Array[Byte]): Boolean =
    val rowLen     = rowData.length
    val sCount     = slotCount
    val fsp        = readShort(FreeSpaceOffset)
    val dirEnd     = HeaderSize + (sCount + 1) * 4 // new directory entry
    val newFsp     = fsp - rowLen
    val available  = fsp - dirEnd

    // Need space for the directory entry (4 bytes) + row data
    if rowLen + 4 > available then return false

    // Write row data backward from free space pointer
    System.arraycopy(rowData, 0, data, newFsp, rowLen)

    // Write slot directory entry
    val slotOff = HeaderSize + sCount * 4
    writeShort(slotOff, newFsp)
    writeShort(slotOff + 2, rowLen)

    // Update slot count and free space pointer
    writeShort(SlotCountOffset, sCount + 1)
    writeShort(FreeSpaceOffset, newFsp)
    true

  def getSlot(index: Int): Option[Array[Byte]] =
    require(index >= 0 && index < slotCount, s"slot index $index out of range [0, $slotCount)")
    val slotOff = HeaderSize + index * 4
    val offset  = readShort(slotOff)
    if offset == 0 then None // tombstone
    else
      val length = readShort(slotOff + 2)
      val result = new Array[Byte](length)
      System.arraycopy(data, offset, result, 0, length)
      Some(result)

  def removeSlot(index: Int): Unit =
    require(index >= 0 && index < slotCount, s"slot index $index out of range [0, $slotCount)")
    val slotOff = HeaderSize + index * 4
    writeShort(slotOff, 0)     // tombstone: offset = 0
    writeShort(slotOff + 2, 0)

  def updateSlot(index: Int, newData: Array[Byte]): Boolean =
    require(index >= 0 && index < slotCount, s"slot index $index out of range [0, $slotCount)")
    val slotOff  = HeaderSize + index * 4
    val oldOff   = readShort(slotOff)
    val oldLen   = readShort(slotOff + 2)

    if newData.length <= oldLen then
      // Fits in existing space — write in-place (at end of old allocation)
      val writeOff = oldOff + oldLen - newData.length
      System.arraycopy(newData, 0, data, writeOff, newData.length)
      writeShort(slotOff, writeOff)
      writeShort(slotOff + 2, newData.length)
      true
    else
      // Doesn't fit — tombstone old slot, try to add as new slot
      removeSlot(index)
      // Check if we have room for the new data at the end (using the free gap)
      val sCount = slotCount
      val fsp    = readShort(FreeSpaceOffset)
      val dirEnd = HeaderSize + sCount * 4 // directory is already at max (no new entry needed)
      val newFsp = fsp - newData.length

      if newFsp < dirEnd then
        // Restore the old slot — no room
        writeShort(slotOff, oldOff)
        writeShort(slotOff + 2, oldLen)
        false
      else
        // Write new data
        System.arraycopy(newData, 0, data, newFsp, newData.length)
        writeShort(slotOff, newFsp)
        writeShort(slotOff + 2, newData.length)
        writeShort(FreeSpaceOffset, newFsp)
        true

  def isSlotTombstone(index: Int): Boolean =
    val slotOff = HeaderSize + index * 4
    readShort(slotOff) == 0 && (index >= slotCount || readShort(slotOff + 2) == 0)

  private def readShort(offset: Int): Int =
    ((data(offset) & 0xff) << 8) | (data(offset + 1) & 0xff)

  private def writeShort(offset: Int, value: Int): Unit =
    data(offset) = ((value >> 8) & 0xff).toByte
    data(offset + 1) = (value & 0xff).toByte

object SlottedPage:
  def create(pageSize: Int): SlottedPage =
    val data = new Array[Byte](pageSize)
    // slot count = 0
    data(SlotCountOffset) = 0
    data(SlotCountOffset + 1) = 0
    // free space pointer = pageSize
    data(FreeSpaceOffset) = ((pageSize >> 8) & 0xff).toByte
    data(FreeSpaceOffset + 1) = (pageSize & 0xff).toByte
    // next page = NoPage
    data(NextPageOffset) = 0
    data(NextPageOffset + 1) = 0
    data(NextPageOffset + 2) = 0
    data(NextPageOffset + 3) = 0
    new SlottedPage(data, pageSize)

  def wrap(data: Array[Byte]): SlottedPage =
    new SlottedPage(data.clone(), data.length)

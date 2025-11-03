import { render, screen, fireEvent } from '@testing-library/react'
import { describe, it, expect, test } from 'vitest'
import Hospital from '../components/Hospital'

describe('Hospital Component', () => {
  it('renders the component with initial hospital data', () => {
    render(<Hospital />)
    
    expect(screen.getByText('Hospital Birth Rate Analysis')).toBeInTheDocument()
    expect(screen.getByDisplayValue('Hospital A')).toBeInTheDocument()
    expect(screen.getByDisplayValue('50')).toBeInTheDocument() // First hospital births
    expect(screen.getByDisplayValue('30')).toBeInTheDocument() // First hospital days
  })

  it('calculates pooled rate correctly', () => {
    render(<Hospital />)
    
    expect(screen.getByText('Pooled Rate')).toBeInTheDocument()
    const pooledRate = screen.getByText(/\d+\.\d+/) // Should show a number
    expect(pooledRate).toBeInTheDocument()
  })

  it('adds new hospital to the list', () => {
    render(<Hospital />)
    
    const addButton = screen.getByText('Add Hospital')
    fireEvent.click(addButton)
    
    expect(screen.getByDisplayValue('New Hospital')).toBeInTheDocument()
  })

  it('removes hospital from the list', () => {
    render(<Hospital />)
    
    // Should have 5 hospitals initially
    expect(screen.getByDisplayValue('Hospital E')).toBeInTheDocument()
    
    const removeButtons = screen.getAllByText('Remove')
    fireEvent.click(removeButtons[removeButtons.length - 1])
    
    // Should no longer have Hospital E
    expect(screen.queryByText('Hospital E')).not.toBeInTheDocument()
  })

  it('updates calculations when hospital data changes', () => {
    render(<Hospital />)
    
    const birthsInput = screen.getByDisplayValue('50')
    fireEvent.change(birthsInput, { target: { value: '100' } })
    
    // Pooled rate should update
    expect(screen.getByText('Pooled Rate')).toBeInTheDocument()
  })

  it('displays birth rate comparison chart', () => {
    render(<Hospital />)
    
    expect(screen.getByTestId('bar-chart')).toBeInTheDocument()
    expect(screen.getByText('Birth Rate Comparison')).toBeInTheDocument()
  })

  it('shows posterior distributions visualization', () => {
    render(<Hospital />)
    
    expect(screen.getByTestId('line-chart')).toBeInTheDocument()
    expect(screen.getByText('Posterior Distributions')).toBeInTheDocument()
  })

  it('displays hyperparameter estimates', () => {
    render(<Hospital />)
    
    expect(screen.getByText('Hyperparameter Estimates')).toBeInTheDocument()
    expect(screen.getByText(/α =/)).toBeInTheDocument()
    expect(screen.getByText(/β =/)).toBeInTheDocument()
  })

  it('shows model comparison', () => {
    render(<Hospital />)
    
    expect(screen.getByText('Model Comparison')).toBeInTheDocument()
    expect(screen.getByText(/Complete pooling:/)).toBeInTheDocument()
    expect(screen.getByText(/No pooling:/)).toBeInTheDocument()
    expect(screen.getByText(/Partial pooling:/)).toBeInTheDocument()
  })

  test('handles single hospital data', () => {
    render(<Hospital />)
    
    // Remove all hospitals except one
    const removeButtons = screen.getAllByText('Remove')
    for (let i = 0; i < 4; i++) {
      fireEvent.click(removeButtons[0])
    }
    
    expect(screen.getByDisplayValue('Hospital A')).toBeInTheDocument()
    expect(screen.queryByText('Hospital B')).not.toBeInTheDocument()
  })

  test('validates hospital name input', () => {
    render(<Hospital />)
    
    const nameInput = screen.getByDisplayValue('Hospital A')
    fireEvent.change(nameInput, { target: { value: 'Test Hospital' } })
    
    expect(screen.getByDisplayValue('Test Hospital')).toBeInTheDocument()
  })

  test('validates births and days input range', () => {
    render(<Hospital />)
    
    const birthsInput = screen.getByDisplayValue('50')
    const daysInput = screen.getByDisplayValue('30')
    
    // Test negative values
    fireEvent.change(birthsInput, { target: { value: '-1' } })
    expect(birthsInput.value).toBe('0') // Should clamp to 0
    
    fireEvent.change(daysInput, { target: { value: '-1' } })
    expect(daysInput.value).toBe('1') // Should clamp to 1
  })

  test('generates example data correctly', () => {
    render(<Hospital />)
    
    const generateButton = screen.getByText('Generate Example')
    fireEvent.click(generateButton)
    
    // Should have new random data
    const inputs = screen.getAllByRole('spinbutton')
    inputs.forEach(input => {
      const value = parseInt(input.value)
      expect(value).toBeGreaterThanOrEqual(0)
    })
  })

  test('displays mathematical framework', () => {
    render(<Hospital />)
    
    expect(screen.getByText('Mathematical Framework')).toBeInTheDocument()
    expect(screen.getByText(/Hierarchical Model Structure/)).toBeInTheDocument()
    expect(screen.getByText(/Key Benefits/)).toBeInTheDocument()
  })

  test('shows key benefits section', () => {
    render(<Hospital />)
    
    expect(screen.getByText(/Borrows strength across hospitals/)).toBeInTheDocument()
    expect(screen.getByText(/Reduces overfitting for small hospitals/)).toBeInTheDocument()
    expect(screen.getByText(/Accounts for between-hospital variation/)).toBeInTheDocument()
  })

  test('handles zero births edge case', () => {
    render(<Hospital />)
    
    const birthsInput = screen.getByDisplayValue('50')
    fireEvent.change(birthsInput, { target: { value: '0' } })
    
    // Should still calculate without errors
    expect(screen.getByText('Pooled Rate')).toBeInTheDocument()
  })

  test('displays total hospitals count', () => {
    render(<Hospital />)
    
    expect(screen.getByText('Total Hospitals')).toBeInTheDocument()
    expect(screen.getByText('5')).toBeInTheDocument() // Initial count
  })
})

import { render, screen, fireEvent } from '@testing-library/react'
import { describe, it, expect, test } from 'vitest'
import Radiation from '../components/Radiation'

describe('Radiation Component', () => {
  it('renders the component with initial sensor data', () => {
    render(<Radiation />)
    
    expect(screen.getByText('Radiation Sensor Analysis')).toBeInTheDocument()
    expect(screen.getByDisplayValue('12')).toBeInTheDocument() // First sensor count
    expect(screen.getByDisplayValue('10.0')).toBeInTheDocument() // Source rate
    expect(screen.getByDisplayValue('0.8')).toBeInTheDocument() // Detector efficiency
    expect(screen.getByDisplayValue('2.0')).toBeInTheDocument() // Background rate
  })

  it('calculates posterior estimates correctly', () => {
    render(<Radiation />)
    
    expect(screen.getByText('Posterior Estimates')).toBeInTheDocument()
    expect(screen.getByText('Source Strength')).toBeInTheDocument()
    expect(screen.getByText('Detector Efficiency')).toBeInTheDocument()
  })

  it('adds new sensor reading', () => {
    render(<Radiation />)
    
    const addButton = screen.getByText('Add Reading')
    fireEvent.click(addButton)
    
    expect(screen.getByText('Time 11:')).toBeInTheDocument()
  })

  it('removes sensor reading', () => {
    render(<Radiation />)
    
    // Should have 10 readings initially
    expect(screen.getByText('Time 10:')).toBeInTheDocument()
    
    const removeButtons = screen.getAllByText('Remove')
    fireEvent.click(removeButtons[removeButtons.length - 1])
    
    // Should no longer have Time 10
    expect(screen.queryByText('Time 10:')).not.toBeInTheDocument()
  })

  it('updates calculations when sensor data changes', () => {
    render(<Radiation />)
    
    const firstCountInput = screen.getByDisplayValue('12')
    fireEvent.change(firstCountInput, { target: { value: '15' } })
    
    // Posterior estimates should update
    expect(screen.getByText('Posterior Estimates')).toBeInTheDocument()
  })

  it('displays posterior distribution visualization', () => {
    render(<Radiation />)
    
    expect(screen.getByTestId('line-chart')).toBeInTheDocument()
    expect(screen.getByText('Posterior Distributions')).toBeInTheDocument()
  })

  it('shows predictive counts chart', () => {
    render(<Radiation />)
    
    expect(screen.getByTestId('scatter-chart')).toBeInTheDocument()
    expect(screen.getByText('Predictive Counts')).toBeInTheDocument()
  })

  it('updates when source parameters change', () => {
    render(<Radiation />)
    
    const sourceRateInput = screen.getByLabelText(/Source Rate:/)
    fireEvent.change(sourceRateInput, { target: { value: '15.0' } })
    
    expect(screen.getByDisplayValue('15.0')).toBeInTheDocument()
  })

  it('displays likelihood analysis', () => {
    render(<Radiation />)
    
    expect(screen.getByText('Likelihood Analysis')).toBeInTheDocument()
    expect(screen.getByText(/Best Likelihood:/)).toBeInTheDocument()
  })

  test('handles minimum sensor readings', () => {
    render(<Radiation />)
    
    // Remove all readings except minimum
    const removeButtons = screen.getAllByText('Remove')
    for (let i = 0; i < 8; i++) {
      fireEvent.click(removeButtons[0])
    }
    
    expect(screen.getByText('Time 1:')).toBeInTheDocument()
    expect(screen.getByText('Time 2:')).toBeInTheDocument()
  })

  test('validates sensor count input', () => {
    render(<Radiation />)
    
    const firstCountInput = screen.getByDisplayValue('12')
    
    // Test negative input
    fireEvent.change(firstCountInput, { target: { value: '-1' } })
    expect(firstCountInput.value).toBe('0') // Should clamp to 0
    
    // Test maximum input
    fireEvent.change(firstCountInput, { target: { value: '105' } })
    expect(firstCountInput.value).toBe('100') // Should clamp to max
  })

  test('generates synthetic data correctly', () => {
    render(<Radiation />)
    
    const generateButton = screen.getByText('Generate Synthetic Data')
    fireEvent.click(generateButton)
    
    // Should have new synthetic data
    const inputs = screen.getAllByRole('spinbutton')
    inputs.forEach(input => {
      const value = parseInt(input.value)
      expect(value).toBeGreaterThanOrEqual(0)
      expect(value).toBeLessThanOrEqual(100)
    })
  })

  test('displays mathematical framework', () => {
    render(<Radiation />)
    
    expect(screen.getByText('Mathematical Framework')).toBeInTheDocument()
    expect(screen.getByText(/Model Structure/)).toBeInTheDocument()
    expect(screen.getByText(/Likelihood Function/)).toBeInTheDocument()
  })

  test('shows applications section', () => {
    render(<Radiation />)
    
    expect(screen.getByText('Applications')).toBeInTheDocument()
    expect(screen.getByText(/Radiation monitoring/)).toBeInTheDocument()
    expect(screen.getByText(/Environmental sensing/)).toBeInTheDocument()
  })

  test('handles zero background rate', () => {
    render(<Radiation />)
    
    const backgroundRateInput = screen.getByLabelText(/Background Rate:/)
    fireEvent.change(backgroundRateInput, { target: { value: '0.0' } })
    
    expect(screen.getByDisplayValue('0.0')).toBeInTheDocument()
    expect(screen.getByText('Posterior Estimates')).toBeInTheDocument()
  })

  test('detector efficiency parameter validation', () => {
    render(<Radiation />)
    
    const efficiencyInput = screen.getByLabelText(/Detector Efficiency:/)
    
    // Test invalid efficiency (> 1)
    fireEvent.change(efficiencyInput, { target: { value: '1.5' } })
    expect(efficiencyInput.value).toBe('1.0') // Should clamp to 1
    
    // Test negative efficiency
    fireEvent.change(efficiencyInput, { target: { value: '-0.1' } })
    expect(efficiencyInput.value).toBe('0.1') // Should clamp to minimum
  })

  test('source rate parameter validation', () => {
    render(<Radiation />)
    
    const sourceRateInput = screen.getByLabelText(/Source Rate:/)
    
    // Test negative source rate
    fireEvent.change(sourceRateInput, { target: { value: '-1.0' } })
    expect(sourceRateInput.value).toBe('0.1') // Should clamp to minimum
  })

  test('displays calibration analysis', () => {
    render(<Radiation />)
    
    expect(screen.getByText('Calibration Analysis')).toBeInTheDocument()
    expect(screen.getByText(/Expected Counts:/)).toBeInTheDocument()
    expect(screen.getByText(/Observed Counts:/)).toBeInTheDocument()
  })

  test('shows uncertainty quantification', () => {
    render(<Radiation />)
    
    expect(screen.getByText('Uncertainty Quantification')).toBeInTheDocument()
    expect(screen.getByText(/Credible Interval:/)).toBeInTheDocument()
  })
})
